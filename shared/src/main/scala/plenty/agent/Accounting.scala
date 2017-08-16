package plenty.agent

import java.util.Date

import plenty.agent.model.Agent
import plenty.state.model.{Node, Transaction}

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
    val now = new Date().getTime
    var s = agent.state
    val coins = s.coins.filter(_.deathTime > now)
    s = s.copy(coins = coins)
    agent.copy(state = s)
  }

  def createTransaction(to: Node, amount: Int)(implicit agent: Agent): Either[InsufficientBalance, Transaction] = ???

  private def canTransactAmount(amount: Int)(implicit agent: Agent): Boolean = {
    canTransactAmount(AgentManager.agentAsNode(agent), agent, amount)
  }

  def canTransactAmount(node: Node, agent: Agent, amount: Int): Boolean = {
    Accounting.getBalance(node)(agent) >= amount
  }

}

class InsufficientBalance extends Exception("Insufficient balance")
