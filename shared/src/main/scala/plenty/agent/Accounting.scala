package plenty.agent

import plenty.agent.model.Agent
import plenty.state.model.{Node, Transaction}

/**
  * Functions related to accounting, such as getting a coin balance for a node, or minting coins
  */
private object Accounting {
  def getBalance(node: Node)(implicit agent: Agent): Int = {
    agent.state.coins.count(_.belongsTo.id == node.id)
  }

  def createTransaction(to: Node, amount: Int)(implicit agent: Agent): Either[InsufficientBalance, Transaction] = ???

  private def canTransactAmount(amount: Int)(implicit agent: Agent): Boolean = {
    Accounting.getBalance(AgentManager.agentAsNode(agent))(agent) >= amount
  }

}

class InsufficientBalance extends Exception("Insufficient balance")
