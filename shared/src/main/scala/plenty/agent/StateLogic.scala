package plenty.agent

import plenty.agent.model._
import plenty.network.communication._
import plenty.state.StateManager
import plenty.state.model.{Bid, Coin, Donation, State}

/**
  * Facilitates interaction between two [[plenty.agent.model.Agent]]
  */
object StateLogic {

  def registerCoins(coins: Set[Coin], agent: Agent): Agent = {
    var s = agent.state
    s = s.copy(coins = s.coins ++ coins)
    val a = agent.copy(state = s)
    StateManager.save(a)
    a
  }

  def registerAcceptedBid(bid: Bid)(implicit agent: Agent): Agent = {
    val history = agent.state.history
    val historyUpdated = history.copy(
      donations = history.donations + bid.donation,
      bids = history.bids + bid
    )

    val stateUpdated = agent.state.copy(
      donations = agent.state.donations.filterNot(_.id == bid.donation.id),
      bids = agent.state.bids.filterNot(_.id == bid.id),
      nonSettledBids = agent.state.nonSettledBids + bid,
      history = historyUpdated
    )
    val agentUpdated = agent.copy(state = stateUpdated)

    StateManager.save(agentUpdated)
    agentUpdated
  }

  def registerBid(bid: Bid)(implicit agent: Agent): Agent = {
    val stateUpdated = agent.state.copy(
      bids = agent.state.bids + bid
    )
    val agentUpd = agent.copy(state = stateUpdated)

    StateManager.save(agentUpd)

    agentUpd
  }

  def donationRegistration(donation: Donation)(implicit agent: Agent): Agent = {
    val stateUpdated = agent.state.copy(
      donations = agent.state.donations + donation
    )
    val agentUpd = agent.copy(state = stateUpdated)

    StateManager.save(agentUpd)

    agentUpd
  }
//  def donationRelayOccured(donation: Donation)(implicit agent: Agent): Agent = {
//    val updatedRelay = agent.state.relay.copy(
//      donations = agent.state.relay.donations + donation
//    )
//    val stateUpdated = agent.state.copy(
//      relay = updatedRelay
//    )
//    val agentUpd = agent.copy(state = stateUpdated)
//
//    StateManager.save(agentUpd)
//
//    agentUpd
//  }


}
