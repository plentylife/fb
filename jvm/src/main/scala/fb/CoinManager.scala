package fb

import java.util.Date

import plenty.agent.Accounting
import plenty.agent.model.Agent
import plenty.network.{ActionIdentifiers, Network}
import plenty.state.StateManager
import plenty.state.model.Coin

/**
  * Creates representations of all possible coins for an [[Agent]]
  */
object MintPress {
  /** the largest possible id of a coin; also equal to the total number of coins in the system */
  private val maxCoinId: Long = 2023L

  private def coinRange = 1L to maxCoinId

  /** given a set of coins checks if there are any global coins missing */
  private def checkForMissingCoins(coins: Set[Coin]): Set[Long] = {
    val ids: Set[Long] = coins.map(_.id)
    coinRange.toSet diff ids
  }

  /** Creates all the valid coins that have not yet been created and amends them to the existing coins */
  def fillCoinSet(existingCoins: Set[Coin]): Set[Coin] = {
    val now = new Date().getTime
    checkForMissingCoins(existingCoins) map { id ⇒
      Coin(id, FbAgent.node, now, now)
    }
  }
}

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
