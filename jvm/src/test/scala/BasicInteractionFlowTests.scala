import plenty.agent.AgentManager
import plenty.agent.model.Agent
import plenty.network.communication.{BidAction, DonateAction, Message}
import plenty.network.{Network, Scheduler}
import plenty.state.StateManager
import plenty.state.model.State
import utest._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  * Saving state, modifying state by agents
  */
object NetworkTests extends TestSuite {

  FastTestScheduler.start()

  val a1 = Agent("a1", state = State())
  val a2 = Agent("a2", state = State())
  val a3 = Agent("a3", state = State())
  val n = Array(AgentManager.agentAsNode(a1), AgentManager.agentAsNode(a2), AgentManager.agentAsNode(a3))

  val donation = StateManager.createDonation("d-title", "d-desc", AgentManager.agentAsNode(a1))
  var bid = StateManager.createBid(donation, amount = 1, by = n(1))
  // for testing purposes fudging the bid timestamp
  bid = bid.copy(timestamp = bid.timestamp - 25 * 60 * 60 * 1000)

  println(s"creating donation $donation")
  println(s"creating bid $bid")

  val tests = this {
    'three_agent_network {

      'registering_agents {
        Network.registerAgent(a1)
        Network.registerAgent(a2)
        Network.registerAgent(a3)
        assert(getAgents.size == 3)
      }

      'donating {
        Network.notifyAll(donation, DonateAction, from = AgentManager.agentAsNode(a1))
        waitClearQueue

        for (a <- getAgents) {
          assert(a.state.donations.contains(donation))
        }
        assert(getAgents.size == 3)
      }

      'bidding {
        assert(bid.donation == donation)
        val msg = Message.createMessage(n(1), n(0), BidAction, bid)

        Network.send(msg)
        waitClearQueue

        for (a <- getAgents) {
          assert(a.state.donations.contains(donation))
          assert(a.state.bids.contains(bid))
          assert(a.state.bids.find(_ == bid).get.donation == donation)
        }

        assert(Network.getAgents.size == 3)
      }

      'bid_acceptance {
        for (a <- getAgents) {
//          println(s"bid_acceptance ${a}")
          assert(a.state.bids.contains(bid))
          assert(a.state.donations.contains(donation))
        }
        Thread.sleep(6000)
        waitClearQueue
        for (a <- getAgents) {
          assert(!a.state.bids.contains(bid))
          assert(!a.state.donations.contains(donation))
          assert(!a.state.nonSettledBids.contains(bid))
        }
      }

    }

    FastTestScheduler.stop()
  }

  def getAgents: Iterable[Agent] = {
    Network.getAgents.map(_.getAgentInLastKnownState)
  }

  def waitClearQueue = {
    println("waiting on message queue")
    println(s"non-completes: ${Network.nonCompletes.mkString(" ")}")
    while (Network.nonCompletes.nonEmpty) {
      Thread.sleep(1000)
      println(s"non-completes (loop): ${Network.nonCompletes.mkString(" ")}")
    }
  }
}

object FastTestScheduler extends Scheduler {
  override val cycleTime = 6000
}