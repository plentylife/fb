package agent

import agent.model.Agent
import communication.Message
import state.model.Node

/**
  * The access point to the agent module
  */
object AgentGuardian {
  def agentAsNode(agent: Agent): Node = {
    Node(agent.id)
  }

  def interact(message: Message[_], thisAgent: Agent): Agent = {
    Interaction.updateState(message)(thisAgent)
  }
}
