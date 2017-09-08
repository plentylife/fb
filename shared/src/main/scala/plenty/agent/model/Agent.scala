package plenty.agent.model

import plenty.agent.AgentManager
import plenty.state.model.{Node, State}

/**
  * The model of an acting participant in the market
  */

case class Agent(node: Node, state: State) {
  def id: String = node.id

  override def equals(o: scala.Any): Boolean = o match {
    case o: Agent => o.id == this.id
    case _ => false
  }
}
