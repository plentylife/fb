package plenty.agent

import java.util.Date

import plenty.agent.model.Agent
import plenty.network.Network
import plenty.network.communication._
import plenty.state.model.{Bid, Donation, Node, Transaction}

/**
  * Logic of agent's actions such as accepting bids, accepting coins, etc
  */
object ActionLogic {
  private val periodBeforeBidAcceptance: Int = 24 * 60 * 60 * 1000

  /**
    * Checks all open bids that can be accepted, and makes a decision whether any of them should be.
    * The current criteria for accepting a bid is that no additional bids were placed on the same donation within the
    * last day
    * */
  def takeBids(agent: Agent): Unit = {
    println(s"agent ${agent.id} looking to accept bids")
    val now = new Date().getTime
    val criteria = takeBidForDonation(now) _
    val bidsByDonation = agent.state.bids.groupBy(_.donation.id)
    val accepted = bidsByDonation.flatMap(kv => {
      val (_, bids) = kv
      criteria(bids)
    })

//    println(s"agent ${agent.id} has accepted ${accepted}")
    val self = AgentManager.agentAsNode(agent)
    for (acceptedBid <- accepted) {
      Network.notifyAll(acceptedBid, ActionIdentifiers.BID_TAKE_ACTION, from = self)
//      CommsManager.toSelf(acceptedBid, BidAcceptAction, self = self)
//      CommsManager.basicRelay(acceptedBid, BidAcceptAction, from = self)
    }
  }

  def transactOnPromisedBids(implicit agent: Agent): Unit = {
    for (bid <- agent.state.nonSettledBids) {
      if (bid.by.id == AgentManager.agentAsNode(agent).id) {
        transact(bid.donation.by, bid.amount, bid)
      }
    }
  }

  def transact(to: Node, amount: Int, bid: Bid)(implicit agent: Agent): Option[InsufficientBalance] = {
    Accounting.createTransaction(to, amount) match {
      case Left(e) => Option(e)
      case Right(_t: Transaction) =>
        // attaching bid
        val t = _t.copy(bid = Some(bid))
        CommsManager.sendTransaction(t)
        None
    }
  }

  private def takeBidForDonation(now: Long)(bids: Iterable[Bid]): Option[Bid] = {
    if (bids.isEmpty) return None

    // has any new bids been submitted in the last day
    val isAuctionClosed = bids.forall(_.timestamp < now + periodBeforeBidAcceptance)
    if (isAuctionClosed) {
      val maxBid = bids.map(_.amount).max
      val highestBids = bids.filter(_.amount >= maxBid)
      val earliestTimestamp = highestBids.map(_.timestamp).min
      highestBids.find(_.timestamp <= earliestTimestamp)
    } else {
      None
    }
  }


  def relayDonation(donation: Donation)(implicit agent: Agent) = {
    if (!agent.state.donations.contains(donation)) {
      //      CommsManager.basicRelay(donation, RelayIdentifiers.DONATION_RELAY, AgentManager.agentAsNode(agent))
    }
  }

  def relayBid(bid: Bid)(implicit agent: Agent) = {
    if (!agent.state.bids.contains(bid)) {
      //      CommsManager.basicRelay(bid, RelayIdentifiers.BID_RELAY, AgentManager.agentAsNode(agent))
    }
  }

}
