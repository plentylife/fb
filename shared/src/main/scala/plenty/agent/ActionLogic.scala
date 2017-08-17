package plenty.agent

import java.util.Date

import plenty.agent.AgentManager.agentAsNode
import plenty.agent.model.Agent
import plenty.network.{ActionIdentifiers, Message, Network}
import plenty.state.model._

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
      Network.notifyAllAgents(acceptedBid, ActionIdentifiers.BID_TAKE_ACTION, from = self)
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
//        CommsManager.sendTransaction(t)
        None
    }
  }

  /* Bidding */

  /** is the bid valid, does the bidder have enough coins?
    * @return true if accepted */
  def verifyBid(bid: Bid, from: Node, a: Agent) = {
    val accept = Accounting.canTransactAmount(bid.by, a, bid.amount)
    if (accept) {
      Network.notifyAllAgents(bid, ActionIdentifiers.ACCEPT_BID_ACTION, a)
    } else {
      val rejection = RejectedBid("low on funds", bid)
      Network.notifyAllAgents(rejection, ActionIdentifiers.REJECT_BID_ACTION, a)
    }
  }

  def bidSettling(t: Transaction, agent: Agent): Unit = {
    if (t.to == agentAsNode(agent) && validateBidSettle(t, agent)) {
      Network.notifyAllAgents(t, ActionIdentifiers.APPROVE_SETTLE_BID_ACTION, AgentManager.agentAsNode(agent))
    } else {
      Network.notifyAllAgents(t, ActionIdentifiers.DENY_SETTLE_BID_ACTION, AgentManager.agentAsNode(agent))
    }
  }

  private def validateBidSettle(t: Transaction, a: Agent): Boolean = {
    t.bid match {
      case Some(tbid) =>
        val bid = a.state.nonSettledBids.find(_ == tbid)
        if (bid.isEmpty) return false
        val coins = a.state.coins.intersect(t.coins)
        if (coins.size < bid.get.amount) return false
        return true
      case _ => false
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

  /* Relays */

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
