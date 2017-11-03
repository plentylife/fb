package plenty.network

import java.util.Date

import plenty.agent.model.Agent
import plenty.state.model.{Coin, Node}

/**
  * Creates representations of all possible coins for an [[Agent]]
  */
object MintPress {
  /** the largest possible id of a coin; also equal to the total number of coins in the system */
  private val maxCoinId: Long = 41000L

  private def coinRange = 1L to maxCoinId

  /** given a set of coins checks if there are any global coins missing */
  private def checkForMissingCoins(coins: Set[Coin]): Set[Long] = {
    val ids: Set[Long] = coins.map(_.id)
    coinRange.toSet diff ids
  }

  /** Creates all the valid coins that have not yet been created and amends them to the existing coins */
  def fillCoinSet(existingCoins: Set[Coin], owner: Node): Set[Coin] = {
    val now = new Date().getTime
    checkForMissingCoins(existingCoins) map { id ⇒
      Coin(id, owner, now, now)
    }
  }
}
