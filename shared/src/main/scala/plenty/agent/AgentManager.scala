package plenty.agent

import plenty.agent.model.Agent
import plenty.network.{Message, MintPress}
import plenty.state.model._
import scala.language.implicitConversions

/**
  * The access point to the agent module
  */
object AgentManager {
  /* Misc registrations */

  def registerNode(node: Node, agent: Agent): Agent = {
    StateLogic.registerNode(node, agent)
  }

  def registerCoins(coins: Set[Coin], agent: Agent): Agent = {
    StateLogic.registerCoins(coins, agent)
  }

  def registerDonation(msg: Message[Donation], toAgent: Agent): Agent = {
    implicit var agent = toAgent
    val donation = msg.payload
    agent = StateLogic.donationRegistration(donation)
    // todo. for now no relaying
    //    ActionLogic.relayDonation(donation)
    agent
  }

  /* Transactions */

  def takeBids(agent: Agent): Unit = ActionLogic.takeBids(agent)

  def onApproveSettleBid(t: Transaction, a: Agent): Agent = {
    // has this been already settled?
    // fixme. t.bid.get is unsafe
    if (a.state.nonSettledBids contains t.bid.get) {
      val newA = StateLogic.registerApprovedBidSettle(t, a)
      ActionLogic.finishTransaction(t, a)
      newA
    } else a
  }

  def onDenySettleBid(t: Transaction, agent: Agent): Agent = {
    // fixme needs to be verified
    val bid = t.bid.get
    val a = StateLogic.removeBid(bid, agent)
    // fixme needs to be verified that the transaction and bid match
    if (t.to == agentAsNode(a)) {
      AgentManager.takeBids(a)
    }
    a
  }

  /* Bids */

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

  def createAgent(id: String): Agent = {
    var a = Agent(id, state = State())
    val coins = MintPress.distributeCoinsToNewAgent(a)
    a = StateLogic.registerCoins(coins, a)
    a
  }

  implicit def agentAsNode(agent: Agent): Node = {
    Node(agent.id)
  }

}
