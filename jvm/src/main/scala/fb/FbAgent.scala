package fb

import plenty.agent.{AgentManager, AgentPointer}
import plenty.agent.model.Agent
import plenty.network.Network
import plenty.state.model.State

/**
  * Created by anton on 8/10/17.
  */
object FbAgent {
  val id = "facebook_agent"

  private var _pointer: AgentPointer = null
  def pointer = _pointer

  /** gets or creates a fb agent */
  def load() = {
    _pointer = Network.getAgents.find(_.id == id).getOrElse(create)
  }

  private def create: AgentPointer = {
    // manual creation as to not create coins
    val a = Agent(id, state = State())
    Network.registerAgent(a)
  }
}
