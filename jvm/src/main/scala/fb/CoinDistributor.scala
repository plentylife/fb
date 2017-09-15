package fb

import plenty.agent.model.Agent
import plenty.agent.{Accounting, AgentPointer, StateLogic}
import plenty.executionContext
import plenty.network.{ActionIdentifiers, MintPress, Network}
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
  def give(p: AgentPointer): Future[Any] = {
    val cs = MintPress.fillCoinSet(FbAgent.lastState.coins, p.node).take(coinsPerAccount)
    if (cs.nonEmpty) {

      val fAll: TraversableOnce[Future[Any]] = Network.getAgents.map { ap =>
        ap.getAgentToModify().map {_ ⇒
          ap.set(StateLogic.registerCoins(cs, ap.agentInLastState))
        }
      }

      Future.sequence(fAll)

    } else {
      Future{}
    }
  }
}
