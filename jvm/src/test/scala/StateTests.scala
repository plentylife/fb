import org.scalatest.FreeSpec
import plenty.agent.model.Agent
import plenty.state.StateManager
import plenty.state.model.{Coin, DemurageTransaction, Node, State}

/**
  * Saving state, modifying state by agents
  */
class StateTests extends FreeSpec {
  "agent" - {
    val node = Node("node_id")
    val t = DemurageTransaction("tid", 0, Set(), node, Node("to"))
    val state = State(nodes = Set(node))
    val agent = Agent(Node("agent_id"), state = state)

    "can_be_saved" in {
      StateManager.save(agent)
    }

    "can_be_loaded" in {
      val agentLoaded = StateManager.load(agent.id).get
      println(agentLoaded)
      assert(agentLoaded.state == agent.state)
    }
  }

  "equating by id" - {
    "should work for coins" in {
      val cid = 1
      val c1 = Coin(cid, Node("fake"), 0, 0)
      val c2 = Coin(cid, Node("ars"), 1, 1)

      assert(c1 == c2)
      assert(Set(c1, c2).size == 1)
      assert((Set(c1) diff Set(c2)).isEmpty )

      print(Set(c1) intersect Set(c2))
      print(Set(c1) diff Set(c2))
    }
  }
}
