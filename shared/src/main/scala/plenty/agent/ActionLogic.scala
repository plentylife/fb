package plenty.agent

import java.util.Date

import plenty.agent.AgentManager.agentAsNode
import plenty.agent.model.Agent
import plenty.network.{ActionIdentifiers, Network}
import plenty.state.StateManager
import plenty.state.model._
import scala.language.postfixOps

/**
  * Logic of agent's actions such as accepting bids, accepting coins, etc
  */
object ActionLogic {
  private val periodBeforeBidAcceptance: Int = 24 * 60 * 60 * 1000

  /* Transacting */

  def transactOnPromisedBids(implicit agent: Agent): Unit = {
    for (bid <- agent.state.nonSettledBids) {
      if (bid.by.id == AgentManager.agentAsNode(agent).id) {
        transact(bid.donation.by, bid.amount, bid)
      }
    }
  }

  private def lowOnFundsRejection(t: Transaction) = RejectedTransaction("low on funds", t)

  def transact(to: Node, amount: Int, bid: Bid)(implicit agent: Agent): Option[InsufficientBalance] = {
    println(s"agent ${agent.id} creating transaction to ${to}")
    Accounting.createTransaction(to, amount) match {
      case Left(e) =>
        e match {
          case _: InsufficientBalance => Network.notifyAllAgents(bid, ActionIdentifiers.RETRACT_BID_ACTION, agent)
          case _ => Unit
        }
        Option(e)
      // fixme send network message
      case Right(_t: Transaction) =>
        // attaching bid
        val t = _t.copy(bid = Some(bid))
        Network.notifyAllAgents(t, ActionIdentifiers.SETTLE_BID_ACTION, agent)
        None
    }
  }

  /**
    * the only agent allowed to approve or deny a bid is the one accepting it
    * */
  def verifyTransactionForBid(t: Transaction, agent: Agent) = {
//    if (t.to == agentAsNode(agent)) {
      t.bid match {
        case Some(bid) =>
          agent.state.nonSettledBids.find(_ == bid) match {
            case Some(trustedBid) => Accounting.verifyTransaction(t, trustedBid.amount, agent) match {
              case None => Network.notifyAllAgents(t, ActionIdentifiers.APPROVE_SETTLE_BID_ACTION, agent)

              case Some(e: InsufficientBalance) => Network.notifyAllAgents(lowOnFundsRejection(t),
                ActionIdentifiers.DENY_SETTLE_BID_ACTION, agent)
            }
            case _ => Network.notifyAllAgents(RejectedTransaction("could not find the bid", t), ActionIdentifiers
              .DENY_SETTLE_BID_ACTION, agent)
          }
        case _ => Network.notifyAllAgents(RejectedTransaction("improper formatting", t), ActionIdentifiers
          .DENY_SETTLE_BID_ACTION, agent)
      }
//    }
  }

  /** manages the minting of new coins
    * checks that the transaction is for the agent */
  def finishTransaction(t: Transaction, agent: Agent) = {
    if (agentAsNode(agent) == t.to) {
      val coins = Accounting.transferCoins(t)
      Network.notifyAllAgents(coins, ActionIdentifiers.COINS_MINTED, agent)
    }
  }

  //  /** given an exception sends out a message to the network */
  //  private def exceptionToNetworkNotification(e: Exception, agent: Agent) = {
  //    case e: InsufficientBalance => Network.notifyAllAgents(RejectedTransaction("low on funds", t),
  //      ActionIdentifiers.DENY_SETTLE_BID_ACTION, agent)
  //  }

  /* Bidding */

  /**
    * Checks all open bids that can be accepted, and makes a decision whether any of them should be.
    * The current criteria for accepting a bid is that no additional bids were placed on the same donation within the
    * last day
    * @param hardAuctionClose disregard the one day wait time
    **/
  def takeBids(agent: Agent, hardAuctionClose: Boolean = false): Unit = {
    println(s"agent ${agent.id} looking to accept bids from ${agent.state.bids}")
    val now = new Date().getTime
    val criteria = takeBidForDonation(now, hardAuctionClose) _
    // bids on donations by the agent
    val bids = agent.state.bids filter {
      _.donation.by == agentAsNode(agent)
    }
    val bidsByDonation = bids.groupBy(_.donation.id)
    val accepted = bidsByDonation.flatMap(kv => {
      val (_, bids) = kv
      criteria(bids)
    })

    //    println(s"agent ${agent.id} has accepted ${accepted}")
    val self = AgentManager.agentAsNode(agent)
    for (acceptedBid <- accepted) {
      Network.notifyAllAgents(acceptedBid, ActionIdentifiers.BID_TAKE_ACTION, from = self)
    }
  }

  /** is the bid valid, does the bidder have enough coins?
    *
    * @return true if accepted */
  def verifyBid(bid: Bid, a: Agent): Unit = {
    val hasFunds = Accounting.canTransactAmount(bid.by, a, bid.amount)
    val bidAmounts = StateManager getRelatedBids(a.state, bid) map {_.amount}
    val highestBid = (bidAmounts + 0) max
    val isHighestBid = bid.amount > highestBid
    val isSelf = bid.by != bid.donation.by
    val donationExists = a.state.donations contains bid.donation

    if (hasFunds && isHighestBid && isSelf && donationExists) {
      Network.notifyAllAgents(bid, ActionIdentifiers.ACCEPT_BID_ACTION, a)
    } else {
      val reason = if (!hasFunds) "low on funds"
      else if (!isHighestBid) s"bid is below highest bid of $highestBid"
      else if (!isSelf) "can't bid on your own donation"
      else if (!donationExists) "donation does not exist"
      else "unknown reason"

      val rejection = RejectedBid(reason, bid)
      Network.notifyAllAgents(rejection, ActionIdentifiers.REJECT_BID_ACTION, a)
    }
  }

  /** hard auction close to disregard the one day wait time */
  private def takeBidForDonation(now: Long, hardAuctionClose: Boolean)(bids: Iterable[Bid]): Option[Bid] = {
    if (bids.isEmpty) return None

    // has any new bids been submitted in the last day
    val isAuctionClosed = hardAuctionClose || bids.forall(_.timestamp < now + periodBeforeBidAcceptance)
    if (isAuctionClosed) {
      val maxBid = bids.map(_.amount).max
      val highestBids = bids.filter(_.amount >= maxBid)
      val earliestTimestamp = highestBids.map(_.timestamp).min
      highestBids.find(_.timestamp <= earliestTimestamp)
    } else {
      None
    }
  }

  //  def bidSettling(t: Transaction, agent: Agent): Unit = {
  //    if (t.to == agentAsNode(agent) && validateBidSettle(t, agent)) {
  //      Network.notifyAllAgents(t, ActionIdentifiers.APPROVE_SETTLE_BID_ACTION, AgentManager.agentAsNode(agent))
  //    } else {
  //      Network.notifyAllAgents(t, ActionIdentifiers.DENY_SETTLE_BID_ACTION, AgentManager.agentAsNode(agent))
  //    }
  //  }

  //  private def validateBidSettle(t: Transaction, a: Agent): Boolean = {
  //    t.bid match {
  //      case Some(tbid) =>
  //        val bid = a.state.nonSettledBids.find(_ == tbid)
  //        if (bid.isEmpty) return false
  //        val coins = a.state.coins.intersect(t.coins)
  //        if (coins.size < bid.get.amount) return false
  //        return true
  //      case _ => false
  //    }
  //  }


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
