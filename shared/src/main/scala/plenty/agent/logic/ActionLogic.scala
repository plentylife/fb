package plenty.agent.logic

import java.util.Date
import java.util.logging.Logger

import plenty.agent.model.Agent
import plenty.agent.{Accounting, TransactionException, WrongTransactionAmount, agentAsNode}
import plenty.network.{ActionIdentifiers, Network, PayloadIdentifier}
import plenty.state.StateManager
import plenty.state.model._

import scala.language.postfixOps

/**
  * Logic of agent's actions such as accepting bids, accepting coins, etc
  */
object ActionLogic {
  private val logger = Logger.getLogger("ActionLogic")

  private val periodBeforeBidAcceptance: Long = plenty.daysToMillis(1)

  /**
    * Sends out a transaction, after several conditions are met:
    * - This bid cannot be in the process of being settled
    * - The bid must be by the acting agent
    * - A valid transaction can be made
    **/
  def transactBid(bid: Bid)(implicit a: Agent): Option[Transaction] = {
    // already being settled?
    val pendingTransactions = a.state.transactionsPendingSettle collect {
      case t: BidTransaction if t.bid == bid ⇒ t
    }
    // if yes, do not settle again
    if (pendingTransactions.nonEmpty) {
      return None
    }

    if (bid.by == a.node) {
      a.state.bidsPendingSettle find {_ == bid} orElse {
        logger.info(s"transactBid: agent ${a.id} failed to find the bid: $bid")
        None
      } flatMap { b ⇒

        Accounting.createTransaction(bid.donation.by, bid.amount) match {
          case Left(e) =>
            e match {
              case _: WrongTransactionAmount =>
                // if transaction fails, retracting bid
                Network.notifyAllAgents(bid, ActionIdentifiers.RETRACT_BID_ACTION, a)
                logger.info(s"Agent ${a.id} did not have the required ${bid.amount} coins")
              case _ =>
                logger.info("While tyring to transact a bid, an unknown exception has occurred")
            }
            None
          case Right(_t: Transaction) =>
            val t = StateManager.transformTransaction(_t, bid = bid)
            sendTransaction(t, ActionIdentifiers.SETTLE_BID_ACTION, a)
            Option(t)
        }
      }
    } else None
  }

  /* Transacting */
  /**
    * Makes sure that a transaction that settles a bid is valid and issues an [[ActionIdentifiers.APPROVE_SETTLE_BID_ACTION]]
    * A transaction is verified if all the coins belong to the sender, and if the transaction amount is at least the
    * bid amount
    *
    * */
  def verifyTransactionForBid(t: BidTransaction, agent: Agent): Unit = {
    agent.state.bidsPendingSettle.find(_ == t.bid) match {
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
  def verifyTransaction(t: Transaction, a: Agent): Unit = {
    Accounting.verifyTransaction(t, a) match {
      case _: Right[_,_] => Network.notifyAllAgents(t, ActionIdentifiers.ACCEPT_TRANSACTION, a)
      case _: Left[_,_] =>
        logger.info(s"transaction ${t.from} -> ${t.to} failed on verify (agent ${a.id}) | ${t.id}")
        Network.notifyAllAgents(RejectedTransaction("coins do not belong to the sender", t),
        ActionIdentifiers.REJECT_TRANSACTION, a)
    }
  }
  /**
    * Checks all open bids that can be accepted, and makes a decision whether any of them should be.
    * The current criteria for accepting a bid is that no additional bids were placed on the same donation within the
    * last day
    * @param hardAuctionClose disregard the one day wait time
    **/
  def takeBids(agent: Agent, hardAuctionClose: Boolean = false): Unit = {
    val now = new Date().getTime
    /** function that selects 0 or 1 bids from a list of bids based on time or prompt*/
    val takingBidsWith = takeBestBidFromSet(now, hardAuctionClose) _
    // bids on donations by the agent
    val bids = agent.state.bids filter {
      _.donation.by == agentAsNode(agent)
    }
    if (bids.isEmpty) return

    logger.fine(s"agent ${agent.id} looking to accept bids from ${bids.map(_.id).mkString("\n")}")
    val bidsByDonation = bids.groupBy(_.donation.id)
    val accepted = bidsByDonation.flatMap(kv => {
      val (_, bids) = kv
      takingBidsWith(bids)
    })

    logger.fine(s"agent ${agent.id} has accepted $accepted")
    val self = agent.node
    for (acceptedBid <- accepted) {
      Network.notifyAllAgents(acceptedBid, ActionIdentifiers.BID_TAKE_ACTION, from = self)
    }
  }
  /** hard auction close to disregard the one day wait time */
  private def takeBestBidFromSet(now: Long, hardAuctionClose: Boolean)(bids: Iterable[Bid]): Option[Bid] = {
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

  /* Bidding */
  private[logic] def applyDemurage(implicit a: Agent): Set[DemurageTransaction] = {
    val ts = Accounting.produceDemurageTransactions(a)
    if (ts.isEmpty) {
      logger.warning(s"Could not send out demurage (${a.id})")
    } else {
      ts foreach { t ⇒
        logger.fine(s"Applied demurrage ${t.from} -> ${t.to} of ${t.coins.size}")
        sendTransaction(t, ActionIdentifiers.TRANSACTION, a)
      }
    }
    ts
  }

  /** is the bid valid, does the bidder have enough coins?
    *
    * @return true if accepted */
  def verifyBid(bid: Bid, a: Agent): Unit = {
    val hasFunds = Accounting.canTransactAmount(bid.by, a, bid.amount)
    val bidAmounts = StateManager getRelatedBids(a.state, bid) map {_.amount}
    val highestBid = (bidAmounts + 0) max
    val isHighestBid = bid.amount > highestBid
    val isNotSelf = bid.by != bid.donation.by
    val donationExists = a.state.donations contains bid.donation

    if (hasFunds && isHighestBid && isNotSelf && donationExists) {
      Network.notifyAllAgents(bid, ActionIdentifiers.ACCEPT_BID_ACTION, a)
    } else {
      val reason = if (!hasFunds) "low on funds"
      else if (!isHighestBid) s"bid is below highest bid of $highestBid"
      else if (!isNotSelf) "can't bid on your own donation"
      else if (!donationExists) "donation does not exist"
      else "unknown reason"

      val rejection = RejectedBid(reason, bid)
      Network.notifyAllAgents(rejection, ActionIdentifiers.REJECT_BID_ACTION, a)
    }
  }
  def sendTransaction[P <: Transaction](t: P, payloadId: PayloadIdentifier[P], a: Agent): Unit = {
    Network.notifyAllAgents(t, payloadId, a.node)
  }

  /* Relays */

  def relayDonation(donation: Donation)(implicit agent: Agent): Unit = {
    if (!agent.state.donations.contains(donation)) {
      //      CommsManager.basicRelay(donation, RelayIdentifiers.DONATION_RELAY, AgentManager.agentAsNode(agent))
    }
  }

  def relayBid(bid: Bid)(implicit agent: Agent): Unit = {
    if (!agent.state.bids.contains(bid)) {
      //      CommsManager.basicRelay(bid, RelayIdentifiers.BID_RELAY, AgentManager.agentAsNode(agent))
    }
  }

  /* Utils */

  private def lowOnFundsRejection[T <: Transaction](t: T) = RejectedTransaction("low on funds", t)
}
