package plenty.agent.model

import plenty.state.model.{EquatableById, Node, State}

/**
  * The model of an acting participant in the market
  */
case class Agent(node: Node, state: State, creationTime: Long) extends EquatableById[String] {
  override val id: String = node.id
}
