package plenty

import java.util.Date

import plenty.agent.model.Agent
import plenty.state.model.{Node, State}

import scala.language.implicitConversions

package object agent {
  /* Utils */

  def createAgent(node: Node, copyState: State = State()): Agent =
    Agent(node, state = copyState, new Date().getTime)


  implicit def agentAsNode(agent: Agent): Node = agent.node
}
