import plenty.agent.AgentManager
import plenty.agent.model.Agent
import plenty.state.StateManager
import plenty.state.model.{Donation, Node, State}
import utest._

/**
  * Saving state, modifying state by agents
  */
class StateTests extends TestSuite {
  val tests = this {
    'agent {
      val node = Node("node_id")
      val state = State(nodes = Set(node))
      val agent = Agent(Node("agent_id"), state = state)

      'can_be_saved {
        StateManager.save(agent)
      }

      'can_be_loaded {
        val agentLoaded = StateManager.load(agent.id)
        println(agentLoaded)
        assert(agentLoaded.state == agent.state)
      }
    }
  }
}
