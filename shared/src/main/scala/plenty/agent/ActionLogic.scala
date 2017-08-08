package plenty.agent

import java.util.Date

import plenty.agent.model.Agent
import plenty.network.communication.{ActionIdentifiers, CommsManager, Message}
import plenty.state.model.{Bid, Donation, Node, Transaction}

/**
  * Logic of agent's actions such as accepting bids, accepting coins, etc
  */
object ActionLogic {
  /**
    * Checks all open bids that can be accepted, and makes a decision whether any of them should be.
    * The current criteria for accepting a bid is that no additional bids were placed on the same donation within the
    * last day
    * */
  def acceptBids(agent: Agent): Unit = {
    val now = new Date().getTime
    val criteria = acceptBidForDonation(now) _
    val bidsByDonation = agent.state.bids.groupBy(_.donation.id)
    val accepted = bidsByDonation.flatMap(kv => {
      val (_, bids) = kv
      criteria(bids)
    })

    for (acceptedBid <- accepted) {
      CommsManager.acceptBidBeacon(acceptedBid, fromAgent = agent)
    }
  }

  /**
    * */
  def relayDonation(donation: Donation)(implicit agent: Agent) = {
    if (!agent.state.donations.contains(donation)) {
      CommsManager.basicRelay(donation, ActionIdentifiers.DONATION_RELAY, AgentManager.agentAsNode(agent))
    }
  }

  def transactOnPromisedBids(implicit agent: Agent) = {
    for (bid <- agent.state.nonSettledBids) {
      if (bid.by.id == AgentManager.agentAsNode(agent).id) {
        transact(bid.donation.by, bid.amount)
      }
    }
  }

  def transact(to: Node, amount: Int)(implicit agent: Agent): Option[InsufficientBalance] = {
    Accounting.createTransaction(to, amount) match {
      case Left(e) => Option(e)
      case Right(t: Transaction) =>
        CommsManager.transact(t, agent)
        None
    }
  }

  private def acceptBidForDonation(now: Long)(bids: Iterable[Bid]): Option[Bid] = {
    if (bids.isEmpty) return None

    // has any new bids been submitted in the last day
    val isAuctionClosed = bids.forall(_.timestamp < now + 24 * 60 * 60 * 1000)
    if (isAuctionClosed) {
      val maxBid = bids.map(_.amount).max
      val highestBids = bids.filter(_.amount >= maxBid)
      val earliestTimestamp = highestBids.map(_.timestamp).min
      highestBids.find(_.timestamp <= earliestTimestamp)
    } else {
      None
    }
  }
}
