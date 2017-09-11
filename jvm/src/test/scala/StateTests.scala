import org.scalatest.FreeSpec
import plenty.agent.model.Agent
import plenty.state.StateManager
import plenty.state.model.{Node, State}

/**
  * Saving state, modifying state by agents
  */
class StateTests extends FreeSpec {
  "agent" - {
    val node = Node("node_id")
    val state = State(nodes = Set(node))
    val agent = Agent(Node("agent_id"), state = state)

    "can_be_saved" in {
      StateManager.save(agent)
    }

    "can_be_loaded" in {
      val agentLoaded = StateManager.load(agent.id)
      println(agentLoaded)
      assert(agentLoaded.state == agent.state)
    }
  }
}
