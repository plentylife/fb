package plenty.agent.logic

import java.util.logging.Logger

import plenty.agent.Accounting
import plenty.agent.model.Agent
import plenty.network.ActionIdentifiers._
import plenty.network.Message
import plenty.state.model._

import scala.language.implicitConversions

/**
  * The access point to the agent module
  */
object AgentActions {
  private val logger = Logger.getLogger("AgentManager")

  /* Demurrage */
  def applyDemurrage(a: Agent): Agent = {
    ActionLogic.applyDemurage(a).foldLeft(a)({ (ua, t) ⇒
      StateLogic.onTransact(t, ua)
    })
  }

  /* Misc registrations */

  def registerNode(node: Node, agent: Agent): Agent = {
    if (node != agent.node) {
      StateLogic.registerNode(node, agent)
    } else agent
  }

  def registerCoins(coins: Set[Coin], agent: Agent): Agent = {
    StateLogic.registerCoins(coins, agent)
  }

  def registerDonation(msg: Message[Donation], toAgent: Agent): Agent = {
    implicit var agent: Agent = toAgent
    val donation = msg.payload
    agent = StateLogic.donationRegistration(donation)
    // todo. for now no relaying
    //    ActionLogic.relayDonation(donation)
    agent
  }

  /* Transactions */

  /**
    * for now just checks if a message came from the receiver and then changes the owner of the coins */
  def onAcceptTransaction(t: Transaction, a: Agent, msg: Message[_]): Agent = {
    if (t.from != msg.from) {
      return a
    }

    finishTransaction(t, a)
  }

  private def finishTransaction(t: Transaction, a: Agent): Agent = {
    val coins = Accounting.transferCoins(t) // fixme adding to transactions should be followed by save
    val au = StateLogic.registerCoins(coins, a)
    StateLogic.onTransactionFinish(t, au)
  }

  /**
    * @param hardAuctionClose disregard the one day wait time
    */
  def takeBids(agent: Agent, hardAuctionClose: Boolean = false): Unit = ActionLogic.takeBids(agent, hardAuctionClose)

  /** both onApproveSettleBid and onDenySettleBid are only enacted iff they originate from the donor -- other
    * messages are just suggestions */
  def onApproveSettleBid(t: BidTransaction, a: Agent, msg: Message[_]): Agent = {
    // has this been already settled?
    val bid = t.bid
    if (bid.donation.by != msg.from) {
      return a
    }

    if (a.state.bidsPendingSettle contains bid) {
      var agentUpd = StateLogic.registerApprovedBidSettle(t, a)
      agentUpd = finishTransaction(t, agentUpd)
      agentUpd
    } else a
  }


  /** both onApproveSettleBid and onDenySettleBid are only enacted iff they originate from the donor -- other
    * messages are just suggestions */
  def onDenySettleBid(t: BidTransaction, agent: Agent, msg: Message[_]): Agent = {
    val bid = t.bid
    if (bid.donation.by != msg.from) {
      return agent
    }

    val a = StateLogic.removeBid(bid, agent)
    // fixme needs to be verified that the transaction and bid match
    // todo change to bid.donation.by
    if (t.to == a.node) {
      AgentActions.takeBids(a)
    }
    a
  }

  /* Bids */

  def acceptBid(a: Agent)(msg: Message[Bid]): Agent = {
    if (msg.from == msg.payload.donation.by) {
      StateLogic.registerBid(msg.payload)(a)
    } else a
  }

  def verifyBid(msg: Message[Bid], agent: Agent): Agent = {
    val bid = msg.payload

    ActionLogic verifyBid(bid, agent)
    agent
    // todo. for now no relaying
    //    ActionLogic.relayBid(bid)
  }

  /**
    * Called after a donor decides to take a bid for a donation and issues [[BID_TAKE_ACTION]]
    * That bid is then transferred into the registry of bids that are not yet settled
    * If the bid is made by this agent, they settle it, or retract it
    **/
  def onBidTake(a: Agent)(msg: Message[Bid]): Agent = {
    a.state.bids find (_ == msg.payload) match {
      case Some(bid) ⇒
        // is taken bid coming from donor? If not nothing to execute
        if (msg.from != bid.donation.by) {
          return a
        }
        // agent update
        implicit var ua: Agent = StateLogic.registerTakenBid(bid, a)
        // if this is the agent's bid, then this is the time to settle it
        // tsansactBid checks for that
        ActionLogic.transactBid(bid) foreach { t ⇒
          // if transaction is successful, adding to pending
          ua = StateLogic.onTransact(t, ua) // agent update
        }
        ua
      case None ⇒
        logger.info(s"onBidTake message did not contain a valid bid: $msg")
        a
    }
  }

  def retractBid(bid: Bid, agent: Agent): Agent = {
    val isBidBeingSettled = agent.state.bidsPendingSettle contains bid
    val a = StateLogic.removeBid(bid, agent)

    if (isBidBeingSettled && bid.donation.by == a.node) {
      takeBids(a)
    }
    a
  }

}
