import plenty.agent.AgentManager
import plenty.agent.model.Agent
import plenty.network.communication.{Message}
import plenty.state.StateManager
import plenty.state.model.{Donation, Node, State}
import utest._

/**
  * Saving state, modifying state by agents
  */
object AgentStateIntegration extends TestSuite {
  val tests = this {
    'agent_play {
      val a1 = Agent("a1", state = State())
      val a2 = Agent("a2", state = State())

      'donating {
//        val donation = Donation("d-title", "d-desc", Agent.agentAsNode(a1))
//        val msg = Message.createMessage(from = a1, to=a2, payload = donation)
//
//        val a1upd = Agent.interact(msg, a1)
//        val a2upd = Agent.interact(msg, a2)
//
//        assert {
//          (a1upd.state.donations contains donation) &&
//          (a2upd.state.donations contains donation)
//        }
      }
    }

    'agent {
      val node = Node("node_id")
      val state = State(nodes = Set(node))
      val agent = Agent("agent_id", state = state)

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
