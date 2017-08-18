package plenty.agent

import java.util.Date

import plenty.agent.model.Agent
import plenty.state.StateManager
import plenty.state.model.{Coin, Node, Transaction}
import AgentManager.agentAsNode
import plenty.network.MintPress
/**
  * Functions related to accounting, such as getting a coin balance for a node, or minting coins
  */
object Accounting {
  def getBalance(node: Node)(implicit agent: Agent): Int = {
    val now = new Date().getTime
    agent.state.coins.count(c => c.belongsTo.id == node.id && c.deathTime > now)
  }

  def getSelfBalance(agent: Agent) = getBalance(AgentManager.agentAsNode(agent))(agent)

  def clearDeadCoins(implicit agent: Agent): Agent = {
    var s = agent.state
    val coins = filterDeadCoins(s.coins)
    s = s.copy(coins = coins)
    agent.copy(state = s)
  }

  def filterDeadCoins(coins: Set[Coin]) = {
    val now = new Date().getTime
    coins.filter(_.deathTime > now)
  }

  def getOwnValidCoins(agent: Agent) = getValidCoinsOf(agent, agent)

  def getValidCoinsOf(whom: Node, agent: Agent) = filterDeadCoins {agent.state.coins filter {_.belongsTo == whom}}

  /* Transactions */

  def createTransaction(to: Node, amount: Int)(implicit agent: Agent): Either[InsufficientBalance, Transaction] = {
    val validSelfCoins = getOwnValidCoins(agent)
    if (validSelfCoins.size < amount) return Left(new InsufficientBalance)
    val transCoins = validSelfCoins.toSeq.sortBy(_.deathTime).take(amount)
    val t = StateManager.createTransaction(transCoins.toSet, from=agent, to=to)
    Right(t)
  }

  def verifyTransaction(transaction: Transaction, forAmount: Int, agent: Agent): Option[InsufficientBalance] = {
    val validFromCoins = getValidCoinsOf(transaction.from, agent)
    val validCoins = transaction.coins intersect validFromCoins
    if (validCoins.size < forAmount) {
      Option(new InsufficientBalance)
    } else None
  }

  def transferCoins(transaction: Transaction): Set[Coin] = {
    val old = transaction.coins
    MintPress.genCoins(old, transaction.to)
  }

  /* Utility */

  private def canTransactAmount(amount: Int)(implicit agent: Agent): Boolean = {
    canTransactAmount(AgentManager.agentAsNode(agent), agent, amount)
  }

  def canTransactAmount(node: Node, agent: Agent, amount: Int): Boolean = {
    Accounting.getBalance(node)(agent) >= amount
  }
}

class InsufficientBalance extends Exception("Insufficient balance")
