package fb

import plenty.agent.{Accounting, AgentPointer}
import plenty.agent.model.Agent
import plenty.network.{ActionIdentifiers, Network}
import plenty.state.StateManager
import plenty.state.model.Node

import scala.concurrent.{Await, Future, Promise}
import plenty.executionContext

import scala.concurrent.duration._

/**
  * Helps transact coins from [[FbAgent]] to another [[Agent]]
  **/
object CoinDistributor {
  /** how many coins to give out at a time */
  val coinsPerAccount = 7

  /** takes coins from [[FbAgent]] and gives them away, as long as [[FbAgent]] has any
    * it also forces the coins into the agents state, thus allowing the agent to instantly know their balance
    * @return a future, upon comletion of which, the agent should know about their coins */
  def give(p: AgentPointer): Future[_] = {
    var f = Future {}
    val cs = Accounting.getOwnCoins(FbAgent.pointer.agentInLastState).take(coinsPerAccount)
    if (cs.nonEmpty) {
      import com.softwaremill.quicklens._

      val promise = Promise[Agent]
      p.getAgentToModify(promise)
      f = promise.future.map {a ⇒
        p.set {
          a.modify(_.state.coins).using(_ ++ cs)
        }
      }
      val t = StateManager.createTransaction(cs, FbAgent.node, p.node)
      Network.notifyAllAgents(t, ActionIdentifiers.TRANSACTION, FbAgent.node)
    }
    f
  }
}
