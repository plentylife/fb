import plenty.agent.AgentManager
import plenty.agent.model.Agent
import plenty.network.Network
import plenty.network.communication.{DonateAction, Message}
import plenty.state.StateManager
import plenty.state.model.{Donation, Node, State}
import utest._

/**
  * Saving state, modifying state by agents
  */
object NetworkTests extends TestSuite {
  val tests = this {
    'three_agent_network {
      val a1 = Agent("a1", state = State())
      val a2 = Agent("a2", state = State())
      val a3 = Agent("a3", state = State())
      Network.registerAgent(a1)
      Network.registerAgent(a2)
      Network.registerAgent(a3)

      assert(Network.getAgents.size == 3)

      'donating {
        val donation = StateManager.createDonation("d-title", "d-desc", AgentManager.agentAsNode(a1))
        val msg = Message.createMessage(fromNode = AgentManager.agentAsNode(a1), toNode = AgentManager.agentAsNode(a2),
          DonateAction, donation)

        Network.send(msg)

        assert(Network.getAgents.size == 3)
      }
    }

  }
}
