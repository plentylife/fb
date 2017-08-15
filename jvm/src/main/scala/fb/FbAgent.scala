package fb

import plenty.agent.{AgentManager, AgentPointer}
import plenty.agent.model.Agent
import plenty.network.Network
import plenty.network.communication.{ActionIdentifiers, Message}
import plenty.state.model.{Node, State}

/**
  * Created by anton on 8/10/17.
  */
object FbAgent {
  val id = "facebook_agent"

  private var _node: Node = null
  private var _pointer: AgentPointer = null
  def pointer = _pointer
  def node = _node

  /** gets or creates a fb agent */
  def load() = {
    _pointer = Network.getAgents.find(_.id == id).getOrElse(create)
    _node = AgentManager.agentAsNode(pointer.getAgentInLastKnownState)
  }

  private def create: AgentPointer = {
    // manual creation as to not create coins
    val a = Agent(id, state = State())
    Network.registerAgent(a)
  }

  def registerNode(toRegister: Node) = {
    val msg = Message.createMessage(null, toNode = _node, msgPayloadId = ActionIdentifiers.REGISTER_NODE,
    msgPayload = toRegister)
    Network.send(msg)
  }
}
