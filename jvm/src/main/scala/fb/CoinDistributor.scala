package fb

import java.util.Date

import plenty.agent.Accounting
import plenty.agent.model.Agent
import plenty.network.{ActionIdentifiers, Network}
import plenty.state.StateManager
import plenty.state.model.Coin

/**
  * Helps transact coins from [[FbAgent]] to another [[Agent]]
  * */
object CoinDistributor {
  /** how many coins to give out at a time */
  val coinsPerAccount = 7

  /** takes coins from [[FbAgent]] and gives them away, as long as [[FbAgent]] has any */
  def give(a: Agent): Unit = {
    val cs = Accounting.getOwnCoins(FbAgent.pointer.agentInLastState).take(coinsPerAccount)
    if (cs.nonEmpty) {
      val t = StateManager.createTransaction(cs, FbAgent.node, a.node)
      Network.notifyAllAgents(t, ActionIdentifiers.TRANSACTION, FbAgent.node)
    }
  }
}
