import agent.model.Agent
import state.StateGuardian
import state.model.{Node, State}
import utest._

/**
  * Saving state, modifying state by agents
  */
object AgentStateIntegration extends TestSuite {
  val tests = this {
    'agent {
      val node = Node("node_id")
      val state = State(nodes = Set(node))
      val agent = Agent("agent_id", state = state)

      'can_be_saved {
        StateGuardian.save(agent)
      }

      'can_be_loaded {
        val agentLoaded = StateGuardian.load(agent.id)
        println(agentLoaded)
        assert(agentLoaded.state == agent.state)
      }
    }



  }
}
