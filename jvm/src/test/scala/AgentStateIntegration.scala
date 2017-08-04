import agent.model.Agent
import state.StateGuardian
import state.model.{Node, State}
import utest._

/**
  * Saving state, modifying state by agents
  */
object AgentStateIntegration extends TestSuite {
  val tests = this {
    'state {
      'can_be_saved {
        val node = Node("node_id")
        val state = State(nodes = Set(node))
        val agent = Agent("agent_id", state = state)
        StateGuardian.save(agent)
      }
    }


  }
}
