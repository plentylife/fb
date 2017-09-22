package plenty.agent

import plenty.agent.model.Agent
import plenty.network.Message
import plenty.state.model._

import scala.language.implicitConversions

/**
  * The access point to the agent module
  */
object AgentManager {
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
    val coins = Accounting.transferCoins(t)  // fixme adding to transactions should be followed by save
    val au = StateLogic.registerCoins(coins, a)
    StateLogic.registerTransaction(t, au)
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


    if (a.state.nonSettledBids contains bid) {
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
    if (t.to == agentAsNode(a)) {
      AgentManager.takeBids(a)
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

  def registerTakenBid(bid: Bid, agent: Agent): Agent = {
    var a = StateLogic.registerTakenBid(bid, agent)
    ActionLogic.transactOnPromisedBids(a)
    a
  }

  def retractBid(bid: Bid, agent: Agent): Agent = {
    val isBidBeingSettled = agent.state.nonSettledBids contains bid
    val a = StateLogic.removeBid(bid, agent)

    if (isBidBeingSettled && bid.donation.by == agentAsNode(a)) {
      takeBids(a)
    }
    a
  }

  /* Utils */

  def createAgent(node: Node, copyState: State = State()): Agent =
    Agent(node, state = copyState)


  implicit def agentAsNode(agent: Agent): Node = agent.node

}
