package plenty

import plenty.agent.model.Agent
import plenty.state.model.{Node, State}

import scala.language.implicitConversions

package object agent {
  /* Utils */

  def createAgent(node: Node, copyState: State = State()): Agent =
    Agent(node, state = copyState)


  implicit def agentAsNode(agent: Agent): Node = agent.node
}
