package fb

import fb.network.FbSendReceiveInterface
import plenty.agent.AgentPointer
import plenty.agent.model.Agent
import plenty.network.{ActionIdentifiers, Network}
import plenty.state.model.{Node, State}

/**
  * Object wrapping a regular [[Agent]], representing the god-like (in ways) agent responsible for all accounts
  * created and used through facebook.
  */
object FbAgent {
  val id = "facebook_agent"

  private val _node: Node = Node(id)
  private var _pointer: AgentPointer = _
  def pointer: AgentPointer = _pointer
  def node: Node = _node

  def lastState: State = pointer.agentInLastState.state

  /** gets or creates a fb agent
    * sets coins */
  def load(): Unit = {
    _pointer = Network.getAgents.find(_.id == id).getOrElse(create)
  }

  private def create: AgentPointer = {
    // manual creation as to not create coins
    val a = Agent(node, state = State())
    Network.registerAgent(a, FbSendReceiveInterface)
  }

  def registerNode(toRegister: Node): Unit = {
    Network.notifyAllAgents(toRegister, ActionIdentifiers.REGISTER_NODE, node)
  }
}
