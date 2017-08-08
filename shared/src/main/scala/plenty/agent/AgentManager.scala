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


  def registerBid(msg: Message[Bid])(implicit toAgent: Agent) = StateLogic.registerBid(msg.payload)
  def acceptBid(msg: Message[Bid])(implicit toAgent: Agent) = StateLogic.acceptBid(msg.payload)
}
