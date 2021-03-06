package plenty.network

import java.security.{MessageDigest, SecureRandom}
import java.util.{Base64, Date}

import plenty.agent.AgentManager
import plenty.agent.model.Agent
import plenty.state.model.{Coin, Node}
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
  private var lastDistributionTime: Long = new Date().getTime

  def nextDistributionTime: Long = lastDistributionTime + period

  def distributeCoins(to: Set[Agent]) = {
    val now = new Date().getTime
    if(lastDistributionTime + period <= now) {
      lastDistributionTime = now
      val coins = genCoins(to)
      /* from=null signifies that it is from the network */
      Network.notifyAllAgents(coins, ActionIdentifiers.COINS_MINTED, from = null)
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
    val id = genCoinId(timestamp)
    val belongsToNode = AgentManager.agentAsNode(belongsTo)
    Coin(id=id, belongsTo = belongsToNode, mintTime = timestamp, deathTime = getDeathTime(timestamp),
      wrapsAround = None, approvedBy = Set())
  }

  /** Generates a new coins from existing coins
    * A new coin has the lifespan twice as long as the previous coin */
  def genCoins(coins: Set[Coin], belongsTo: Node): Set[Coin] = {
    val timestamp = new Date().getTime
    coins map {old =>
      val id = genCoinId(timestamp)
      val lifespan = (old.deathTime - old.mintTime) * 2
      val deathTime = timestamp + lifespan
      Coin(id, belongsTo=belongsTo, mintTime = timestamp, deathTime = deathTime, wrapsAround = Option(old),
        approvedBy = Set())
    }
  }


  private def getDeathTime(from: Long) = nextDistributionTime

  private val generator = new SecureRandom()
  private val hasher = MessageDigest.getInstance("SHA-512")
  private def genCoinId(nonce: Long): String = {
    coinCounter += 1
    val rv = new Array[Byte](512)
    generator.nextBytes(rv)
    val idv = rv ++ (nonce + coinCounter).toString.map(_.toByte)
    val hash = hasher.digest(idv)
    Base64.getEncoder.encodeToString(hash)
  }
}

object MintPress extends MintPress
