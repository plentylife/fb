package fb

import plenty.agent.AgentPointer
import plenty.agent.logic.StateLogic
import plenty.agent.model.Agent
import plenty.executionContext
import plenty.network.{MintPress, Network}

import scala.concurrent.Future

/**
  * Helps transact coins from [[FbAgent]] to another [[Agent]]
  **/
object CoinDistributor {
  /** how many coins to give out at a time to a new account */
  val coinsPerAccount = 1

  val coinsPerDonation = 5

  val coinsPerShare = 1

  def giveNewAgent(p: AgentPointer) = give(p, coinsPerAccount)

  def giveForDonating(p: AgentPointer) = give(p, coinsPerDonation)

  def giveForSharing(p: AgentPointer) = give(p, coinsPerShare)


  /** takes coins from [[FbAgent]] and gives them away, as long as [[FbAgent]] has any
    * it also forces the coins into the agents state, thus allowing the agent to instantly know their balance
    *
    * @return a future, upon comletion of which, the agent should know about their coins */
  private def give(p: AgentPointer, howMany: Int): Future[Any] = {
    val cs = MintPress.fillCoinSet(FbAgent.lastState.coins, p.node).take(howMany)
    if (cs.nonEmpty) {

      // setting for all agents
      val fAll: TraversableOnce[Future[Any]] = (Network.getAgents + p).map { ap =>
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
