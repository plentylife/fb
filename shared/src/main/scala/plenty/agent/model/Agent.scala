package plenty.agent.model

import plenty.agent.AgentManager
import plenty.state.model.State

/**
  * The model of an acting participant in the market
  */
case class Agent(id: String, state: State) {
  override def equals(o: scala.Any): Boolean = o match {
    case o: Agent => o.id == this.id
    case _ => false
  }
}
