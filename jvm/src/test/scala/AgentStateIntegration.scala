import agent.AgentGuardian
import agent.model.Agent
import communication.DonateMessage
import state.StateGuardian
import state.model.{Donation, Node, State}
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
        val donation = Donation("d-title", "d-desc", AgentGuardian.agentAsNode(a1))
        val msg = DonateMessage(from = a1, to=a2, payload = donation)

        val a1upd = AgentGuardian.interact(msg, a1)
        val a2upd = AgentGuardian.interact(msg, a2)

        assert {
          (a1upd.state.donations contains donation) &&
          (a2upd.state.donations contains donation)
        }
      }
    }

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
