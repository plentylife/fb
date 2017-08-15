package plenty.network

import java.security.{MessageDigest, SecureRandom}
import java.util.{Base64, Date}

import plenty.agent.AgentManager
import plenty.agent.model.Agent
import plenty.network.communication.ActionIdentifiers
import plenty.state.model.Coin
import sun.misc.BASE64Encoder

/**
  * Class for minting of new coins
  */
trait MintPress {
  /** number of coins per [[plenty.agent.model.Agent]] per [[plenty.network.MintPress.period]] */
  val coinsPerPeriod = 10
  /** how often to distribute the coins and coincidentally how long a freshly minted coin lives. */
  val period: Int = (3 * 24 + 12) * 60 * 60 * 1000

  /** incremented for every coin generated. never reset. */
  private var coinCounter: Long = 0

  /** time in unix epoch of the last time */
  private var lastDistributionTime: Long = 0

  def distributeCoins(to: Set[Agent]) = {
    val now = new Date().getTime
    if(lastDistributionTime + period <= now) {
      val coins = genCoins(to)
      /* from=null signifies that it is from the network */
      Network.notifyAllAgents(coins, ActionIdentifiers.COINS_MINTED, from = null)
      lastDistributionTime = now
    }
  }

  def distributeCoinsToNewAgent(to: Agent): Set[Coin] = {
    val coins = genCoins(Set(to))
    Network.notifyAllAgents(coins, ActionIdentifiers.COINS_MINTED, from = null)
    coins
  }

  /** Creates the coin models in quantities enough for all participants in the network */
  private def genCoins(agents: Set[Agent]): Set[Coin] = {
    val now = new Date().getTime
    agents flatMap (a => {
      (0 until coinsPerPeriod).map(_ => genCoin(a, now))
    })
  }

  /** Generates a single coin */
  private def genCoin(belongsTo: Agent, timestamp: Long): Coin = {
    coinCounter += 1
    val id = genCoinId(timestamp + coinCounter)
    val belongsToNode = AgentManager.agentAsNode(belongsTo)
    Coin(id=id, belongsTo = belongsToNode, mintTime = timestamp, deathTime = getDeathTime(timestamp),
      wrapsAround = None, approvedBy = Set())
  }

  private def getDeathTime(from: Long) = from + period

  private val generator = new SecureRandom()
  private val hasher = MessageDigest.getInstance("SHA-512")
  private def genCoinId(nonce: Long): String = {
    val rv = new Array[Byte](512)
    generator.nextBytes(rv)
    val idv = rv ++ nonce.toString.map(_.toByte)
    val hash = hasher.digest(idv)
    Base64.getEncoder.encodeToString(hash)
  }
}

object MintPress extends MintPress
