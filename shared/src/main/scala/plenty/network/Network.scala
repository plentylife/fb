package plenty.network

import plenty.agent.model.Agent
import plenty.network.communication.{CommsManager, Message}

/**
  * Created by anton on 8/8/17.
  */
object Network {
  // registering sender method
  CommsManager.sender = send
  CommsManager.getAllAgentsInNetwork = () => agents

  def send(msg: Message[_]): Unit = {
    Network.receive(msg)
  }

  def receive(msg: Message[_]) = {
    val agent = CommsManager.receive(msg, toAgent = agents.find(_.id == msg.to.id).get)
    registerAgent(agent)
  }

  private var agents: Set[Agent] = Set()
  def registerAgent(agent: Agent) = {
    agents += agent
  }

  /** for testing purposes fixme */
  def getAgents = agents
}
