package fb

import plenty.agent.{AgentManager, AgentPointer}
import plenty.agent.model.Agent
import plenty.network.{ActionIdentifiers, Message, Network}
import plenty.network.ActionIdentifiers
import plenty.state.model.{Node, State}

import scala.concurrent.Promise
import plenty.executionContext

/**
  * Created by anton on 8/10/17.
  */
object FbAgent {
  val id = "facebook_agent"

  private val _node: Node = Node(id)
  private var _pointer: AgentPointer = null
  def pointer = _pointer
  def node = _node

  def lastState = pointer.agentInLastState.state

  /** gets or creates a fb agent
    * sets coins */
  def load() = {
    _pointer = Network.getAgents.find(_.id == id).getOrElse(create)
    val p = Promise[Agent]()
    pointer.getAgentToModify(p)
    p.future map {a ⇒
      val filledCoins = MintPress.fillCoinSet(a.state.coins)
      val s = a.state.copy(coins = filledCoins)
      pointer.set(a.copy(state = s))
    }
  }

  private def create: AgentPointer = {
    // manual creation as to not create coins
    val a = Agent(node, state = State())
    Network.registerAgent(a, FbSendReceiveInterface$)
  }

  def registerNode(toRegister: Node) = {
    val msg = Message.createMessage(null, toNode = _node, msgPayloadId = ActionIdentifiers.REGISTER_NODE,
    msgPayload = toRegister)
    Network.send(msg)
  }
}
