package agent.model

import state.model.State

/**
  * The model of an acting participant in the market
  */
case class Agent(id: String, state: State)
