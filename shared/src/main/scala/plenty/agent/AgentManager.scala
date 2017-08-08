package plenty.agent

import plenty.agent.model.Agent
import plenty.network.communication.Message
import plenty.state.model.{Bid, Donation, Node}

/**
  * The access point to the agent module
  */
object AgentManager {
  def agentAsNode(agent: Agent): Node = {
    Node(agent.id)
  }


  def registerDonation(donation: Donation, toAgent: Agent): Agent = {
    implicit var agent = toAgent
    ActionLogic.relayDonation(donation)
    agent = StateLogic.donationRegistration(donation)

    agent
  }


  def registerBid(bid: Bid, toAgent: Agent): Agent = {
    implicit var agent = toAgent

    ActionLogic.relayBid(bid)
    agent = StateLogic.registerBid(bid)
    agent
  }
  def registerAcceptedBid(bid: Bid)(implicit toAgent: Agent) = StateLogic.registerAcceptedBid(bid)

  def acceptBids(agent: Agent) = ActionLogic.acceptBids(agent)
}
