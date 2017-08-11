package plenty.agent

import plenty.agent.model.Agent
import plenty.network.MintPress
import plenty.state.model._

/**
  * The access point to the agent module
  */
object AgentManager {
  def createAgent(id: String): Agent = {
    var a = Agent(id, state = State())
    val coins = MintPress.distributeCoinsToNewAgent(a)
    a = StateLogic.registerCoins(coins, a)
    a
  }

  def agentAsNode(agent: Agent): Node = {
    Node(agent.id)
  }

  def registerCoins(coins: Set[Coin], agent: Agent): Agent = {
    StateLogic.registerCoins(coins, agent)
  }

  def registerDonation(donation: Donation, toAgent: Agent): Agent = {
    implicit var agent = toAgent
    // todo. for now no relaying
    //    ActionLogic.relayDonation(donation)
    agent = StateLogic.donationRegistration(donation)

    agent
  }

  def registerBid(bid: Bid, toAgent: Agent): Agent = {
    implicit var agent = toAgent
    // todo. for now no relaying
    //    ActionLogic.relayBid(bid)
    agent = StateLogic.registerBid(bid)
    agent
  }

  def registerTakenBid(bid: Bid, agent: Agent): Agent = {
    var a = StateLogic.registerTakenBid(bid, agent)
    ActionLogic.transactOnPromisedBids(a)
    a
  }

  def takeBids(agent: Agent) = ActionLogic.takeBids(agent)

  def registerBidSettlingTransaction(t: Transaction, agent: Agent) = ActionLogic.bidSettling(t, agent)
}
