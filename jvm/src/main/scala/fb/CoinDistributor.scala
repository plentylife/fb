package fb

import plenty.agent.model.Agent
import plenty.agent.{Accounting, AgentPointer, StateLogic}
import plenty.executionContext
import plenty.network.{ActionIdentifiers, Network}
import plenty.state.StateManager

import scala.concurrent.{Future, Promise}

/**
  * Helps transact coins from [[FbAgent]] to another [[Agent]]
  **/
object CoinDistributor {
  /** how many coins to give out at a time */
  val coinsPerAccount = 7

  /** takes coins from [[FbAgent]] and gives them away, as long as [[FbAgent]] has any
    * it also forces the coins into the agents state, thus allowing the agent to instantly know their balance
    *
    * @return a future, upon comletion of which, the agent should know about their coins */
  def give(p: AgentPointer): Future[Unit] = {
    var f = Future {}
    var cs = Accounting.getOwnCoins(FbAgent.pointer.agentInLastState).take(coinsPerAccount)
    if (cs.nonEmpty) {

      val promise = Promise[Agent]
      val t = StateManager.createTransaction(cs, FbAgent.node, p.node)
      cs = Accounting.transferCoins(t)
      p.getAgentToModify(promise)
      f = promise.future.map { a ⇒
        p.set {
          StateLogic.registerCoins(cs, a)
          // fixme add StateLogic.registerCoins(cs, FbAgent.
        }
      }
      Network.notifyAllAgents(t, ActionIdentifiers.TRANSACTION, FbAgent.node)
    }
    f
  }
}
