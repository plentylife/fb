import plenty.agent.{Accounting, AgentManager, AgentPointer, Scheduler}
import plenty.agent.model.Agent
import plenty.network.BidAction
import plenty.network._
import plenty.state.StateManager
import plenty.state.model.State
import utest._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  * Saving state, modifying state by agents
  */
object BasicInteractionFlowTests extends TestSuite {

  FastTestScheduler.start()

  val a = (0 until 3).map(i => Agent(s"a$i", State()))
  var ap = Seq[AgentPointer]()
  val n = a map AgentManager.agentAsNode

  val donation = StateManager.createDonation("d-title", "d-desc", Seq(), AgentManager.agentAsNode(a(0)))
  var bid = StateManager.createBid(donation, amount = 1, by = n(1))
  var balances = Seq[Int]()
  // for testing purposes fudging the bid timestamp
  bid = bid.copy(timestamp = bid.timestamp - 25 * 60 * 60 * 1000)

  println(s"creating donation $donation")
  println(s"creating bid $bid")

  val tests = this {
    'three_agent_network {

      'registering_agents {
        ap = a map {_a => Network.registerAgent(_a, MockSendInterface)}
        a foreach MintPress.distributeCoinsToNewAgent
        assert(getAgents.size == 3)

        waitClearQueue
        balances = ap map {_.getAgentInLastKnownState} map {Accounting.getSelfBalance}
        println(s"balances ${balances}")
      }

      'donating {
        Network.notifyAllAgents(donation, DonateAction, from = AgentManager.agentAsNode(a(0)))
        waitClearQueue

        for (a <- getAgents) {
          assert(a.state.donations.contains(donation))
        }
        assert(getAgents.size == 3)
      }

      'bidding {
        println("=== bidding")
        assert(bid.donation == donation)
        val msg = Message.createMessage(n(1), n(0), BidAction, bid)

        Network.send(msg)
        waitClearQueue

        for (a <- getAgents) {
          val bids = a.state.bids
          assert(a.state.donations.contains(donation))
          assert(bids.contains(bid))
          assert(a.state.bids.find(_ == bid).get.donation == donation)
        }

        assert(Network.getAgents.size == 3)
      }

      'bid_acceptance {
        for (a <- getAgents) {
//          println(s"bid_acceptance ${a}")
          val bids = a.state.bids
          assert(bids.contains(bid))
          assert(a.state.donations.contains(donation))
        }
        Thread.sleep(6000)
        waitClearQueue
        for (a <- getAgents) {
          assert(!a.state.bids.contains(bid))
          assert(!a.state.donations.contains(donation))
          assert(!a.state.nonSettledBids.contains(bid))
        }

        val newBalances = ap map {_.getAgentInLastKnownState} map Accounting.getSelfBalance
        newBalances(0) - balances(0) ==> 1
        newBalances(1) - balances(1) ==> -1
        ap(0).getAgentInLastKnownState.state.coins exists {_.wrapsAround.nonEmpty}
      }

    }

    'cleanup {
      FastTestScheduler.stop()
    }
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