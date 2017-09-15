package plenty.agent

import java.util.Date

import plenty.agent.AgentManager.agentAsNode
import plenty.agent.model.Agent
import plenty.state.StateManager
import plenty.state.model._

import scala.util.Random

/**
  * Functions related to accounting, such as getting a coin balance for a node, or minting coins
  */
object Accounting {

  val demurageRateCalculatePeriod: Long = 30 * 24 * 60 * 60 * 1000L

  val demuragePeriod: Long = 24 * 60 * 60 * 1000L

  /** Figures out if the agents should give away coins, and gives them away */
  def produceDemurageTransactions(agent: Agent): Set[DemurageTransaction] = {
    val howMuch = Math.floor(calculateDemurage(agent))
    val cs = getOwnCoins(agent).take(howMuch.toInt)
    cs flatMap { c ⇒
      selectNodeForDemurage(agent) map { to ⇒ StateManager.createTransaction(Set(c), from = agent.node, to = to)
      } map {StateManager.asDemurage}
    }
  }

  def selectNodeForDemurage(agent: Agent): Option[Node] = {
    Random.shuffle(agent.state.nodes.toSeq).headOption
  }

  def calculateDemurage(a: Agent, atTime: Long = new Date().getTime): Double = {
    val coins = getOwnCoins(a)
    val lastDemurageTime = getLastDemurageTime(a)
    val rate = calculateDemurageRate(a)

    val perCoin = coins.toList map { c ⇒
      // dumurage accrue start time
      lastDemurageTime getOrElse c.lastTransactionTime
    } map { st ⇒
      // periods
      (atTime - st) / demuragePeriod.toDouble
    } map { p ⇒
      // per coin demurage
      1 - Math.pow(1 - rate, p)
    }
    perCoin.sum
  }

  /** The mathematical function for calculating the demurage rate given the ratio of currency flow out / flow in
    * Linear function, 0.1 at 0, 0.01 at 1, and 0 at 2
    * */
  def demurageRateFunction(flowRatio: Double): Double = {
    val stage1 = 0.05
    val s2 = 0.005
    val diff = stage1 - s2
    if (flowRatio <= 1) {
      stage1 - diff * flowRatio
    } else if (flowRatio < 2) {
      s2 - s2 * (flowRatio - 1)
    } else 0.0
  }

  def calculateDemurageRate(a: Agent): Double = {
    val now = new Date().getTime
    val flow = getOwnTransactions(a) filter {
      _.timestamp > now - demurageRateCalculatePeriod
    }
    val flowIn = flow.filter(_.to == a.node).map(_.coins.size).sum
    val flowOut = flow.filter(_.from == a.node).map(_.coins.size).sum
    // avoiding div by 0
    val ratio = if (flowIn != 0) flowOut.toDouble / flowIn else 0
    demurageRateFunction(ratio)
  }

  def getLastDemurageTime(a: Agent): Option[Long] = a.state.chains.transactions.collectFirst {
    case t if t.transactionType == TransactionType.DEMURAGE ⇒ t.timestamp
  }

  /* Balances */

  /** filters coins based on belonging to the specified node
    *
    * @return the count of filtered coins */
  def getBalance(node: Node)(implicit agent: Agent): Int = {
    val now = new Date().getTime
    agent.state.coins.count(c => c.belongsTo.id == node.id)
  }

  /** @return the balance that the given agent believes they have */
  def getSelfBalance(agent: Agent): Int = getBalance(AgentManager.agentAsNode(agent))(agent)

  def getOwnCoins(a: Agent): Set[Coin] = a.state.coins.filter(c ⇒ c.belongsTo.id == a.id)

  def getCoins(of: Node, inStateOf: Agent): Set[Coin] =
    inStateOf.state.coins.filter(c ⇒ c.belongsTo.id == of.id)

  /* Transactions */

  def getOwnTransactions(a: Agent): List[Transaction] = a.state.chains.transactions filter {
    t ⇒ t.to == a.node || t.from == a.node
  }

  def createTransaction(to: Node, amount: Int)(implicit agent: Agent): Either[WrongTransactionAmount, Transaction] = {
    val coins = getOwnCoins(agent)
    if (!canTransactAmount(amount)) return Left(new WrongTransactionAmount)
    val transCoins = coins.take(amount)
    val t = StateManager.createTransaction(transCoins, from = agent, to = to)
    Right(t)
  }

  /** checks if the coins actually belong to the sender and that the amount is correct */
  def verifyTransaction(transaction: Transaction, agent: Agent): Either[CoinsDoNotBelongToSender, Unit] = {
//    val coinIds = transaction.coins.map {_.id}
    val verifiedCoins = agent.state.coins intersect transaction.coins
    if (verifiedCoins.nonEmpty && verifiedCoins.forall(_.belongsTo == transaction.from))
      Right(Unit)
    else
      Left(new CoinsDoNotBelongToSender())
  }

  def verifyTransactionAmount(t: Transaction, forAmount: Int): Either[WrongTransactionAmount, Unit] = {
    if (t.coins.size >= forAmount)
      Right(Unit)
    else
      Left(new WrongTransactionAmount())
  }

  /** changes ownership to the receiver of the transaction, as well as indicates transaction time on the coins */
  def transferCoins(transaction: Transaction): Set[Coin] = {
    transaction.coins map {_.copy(belongsTo = transaction.to, lastTransactionTime = transaction.timestamp)}
  }

  private def canTransactAmount(amount: Int)(implicit agent: Agent): Boolean = {
    canTransactAmount(AgentManager.agentAsNode(agent), agent, amount)
  }

  def canTransactAmount(node: Node, agent: Agent, amount: Int): Boolean = {
    Accounting.getBalance(node)(agent) >= amount
  }
}

abstract class TransactionException(msg: String) extends Exception(msg)

class WrongTransactionAmount extends TransactionException("Insufficient balance")

class CoinsDoNotBelongToSender extends TransactionException("Some or all coins do not belong to the sender")