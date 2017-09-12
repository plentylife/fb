package plenty.agent

import java.util.Date
import java.util.logging.Logger

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
  private val log = Logger.getLogger("ActionLogic")

  private val periodBeforeBidAcceptance: Int = 24 * 60 * 60 * 1000

  /* Demurage */

  def applyDemurage(implicit a: Agent): Unit = {
    Accounting.produceDemurageTransaction(a) match {
      case None ⇒ log.warning("Could not send out demurage")
      case Some(t) ⇒
        if (t.coins.nonEmpty) {
          log.fine(s"Applied demurrage ${t.from} -> ${t.to} of ${t.coins.size}")
          Network.notifyAllAgents(t, ActionIdentifiers.TRANSACTION, a.node)
        }
    }
  }

  /* Transacting */

  def transactOnPromisedBids(implicit agent: Agent): Unit = {
    for (bid <- agent.state.nonSettledBids) {
      if (bid.by.id == AgentManager.agentAsNode(agent).id) {
        transactBid(bid)
      }
    }
  }

  def transactBid(bid: Bid)(implicit agent: Agent): Option[WrongTransactionAmount] = {
    Accounting.createTransaction(bid.donation.by, bid.amount) match {
      case Left(e) =>
        e match {
          case _: WrongTransactionAmount =>
            // if transaction fails, retracting bid
            Network.notifyAllAgents(bid, ActionIdentifiers.RETRACT_BID_ACTION, agent)
          case _ => Unit
        }
        Option(e)
      case Right(_t: Transaction) =>
        val t = StateManager.transformTransaction(_t, bid = bid)
        Network.notifyAllAgents(t, ActionIdentifiers.SETTLE_BID_ACTION, agent)
        None
    }
  }

  /**
    * Makes sure that a transaction that settles a bid is valid and issues an [[ActionIdentifiers.APPROVE_SETTLE_BID_ACTION]]
    * */
  def verifyTransactionForBid(t: BidTransaction, agent: Agent) = {
    agent.state.nonSettledBids.find(_ == t.bid) match {
      case Some(trustedBid) =>
        var v: Either[TransactionException, Unit] = Accounting.verifyTransaction(t, agent)
        v = v.flatMap(_ ⇒ Accounting.verifyTransactionAmount(t, trustedBid.amount))
      v match {
        case v: Right[_,_] => Network.notifyAllAgents(t, ActionIdentifiers.APPROVE_SETTLE_BID_ACTION, agent)

        case v: Left[_,_] => Network.notifyAllAgents(lowOnFundsRejection(t),
          ActionIdentifiers.DENY_SETTLE_BID_ACTION, agent)
      }
      case _ => Network.notifyAllAgents(RejectedTransaction("could not find the bid", t), ActionIdentifiers
        .DENY_SETTLE_BID_ACTION, agent)
    }
  }

  /**
    * Verifies that the transaction advertises coins that indeed belong to the sender of the transaction
    * Issues an [[ActionIdentifiers.ACCEPT_TRANSACTION]] or [[ActionIdentifiers.REJECT_TRANSACTION]]
    **/
  // fixme. must verify that the sender indeed issued the transaction
  def verifyTransaction(t: Transaction, a: Agent) = {
    Accounting.verifyTransaction(t, a) match {
      case _: Right[_,_] => Network.notifyAllAgents(t, ActionIdentifiers.ACCEPT_TRANSACTION, a)
      case _: Left[_,_] =>
        log.info(s"transaction ${t.from} -> ${t.to} failed on verify (agent ${a.id}) | ${t.id}")
        Network.notifyAllAgents(RejectedTransaction("coins do not belong to the sender", t),
        ActionIdentifiers.REJECT_TRANSACTION, a)
    }
  }

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
    val takenBids = takeBidForDonation(now, hardAuctionClose) _
    // bids on donations by the agent
    val bids = agent.state.bids filter {
      _.donation.by == agentAsNode(agent)
    }
    val bidsByDonation = bids.groupBy(_.donation.id)
    val accepted = bidsByDonation.flatMap(kv => {
      val (_, bids) = kv
      takenBids(bids)
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
    val isAuctionClosed = hardAuctionClose || bids.forall(_.timestamp < now - periodBeforeBidAcceptance)
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

  /* Utils */

  private def lowOnFundsRejection[T <: Transaction](t: T) = RejectedTransaction("low on funds", t)
}
