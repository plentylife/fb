import org.scalatest.{FreeSpec, Matchers}
import plenty.MockSendReceiveInterface
import plenty.TestUtilities._
import plenty.agent._
import plenty.agent.model.Agent
import plenty.network.{BidAction, _}
import plenty.state.StateManager
import plenty.state.model.{Coin, Node, State}

import scala.language.postfixOps
/**
  * Saving state, modifying state by agents
  */
class BasicInteractionFlowTests extends FreeSpec with Matchers {

  FastTestScheduler.start()

  val coinPerAgent = 1
  val ns = (0 until 3) map {i ⇒ Node(i.toString)}
  var cs: Set[Coin] = MintPress.fillCoinSet(Set(), ns(0))
  cs = cs.grouped(coinPerAgent).toSeq zip ns flatMap { case (c, n) ⇒
    c map {_.copy(belongsTo = n)}
  } toSet

  val as = ns map {n =>Agent(n, State(coins = cs))}
  var aps = Seq[AgentPointer]()

  val donation = StateManager.createEmptyDonation(as(0).node)
  var bid = StateManager.createBid(donation, amount = 1, by = ns(1))
  var balances = Seq[Int]()
  // for testing purposes fudging the bid timestamp
  bid = bid.copy(timestamp = bid.timestamp - 25 * 60 * 60 * 1000)

  println(s"creating donation $donation")
  println(s"creating bid $bid")

    "three_agent_network" - {

      "registering_agents" in {
        aps = as map { _a => Network.registerAgent(_a, MockSendReceiveInterface)}
        ns foreach {n ⇒
          Network.notifyAllAgents(n, ActionIdentifiers.REGISTER_NODE, null)
        }
        assert(getAgents.size == 3)

        waitClearQueue()
        balances = aps map {_.agentInLastState} map {Accounting.getSelfBalance}
        balances foreach {b ⇒ b shouldBe coinPerAgent}
        println(s"balances ${balances}")
      }

      "donating" in {
        Network.notifyAllAgents(donation, DonateAction, from = as(0).node)
        waitClearQueue()

        for (a <- getAgents) {
          assert(a.state.donations.contains(donation))
        }
        assert(getAgents.size == 3)
      }

      "bidding" in {
        assert(bid.donation == donation)
        val msg = Message.createMessage(ns(1), ns(0), BidAction, bid)

        Network.send(msg)
        waitClearQueue()

        for (a <- getAgents) {
          val bids = a.state.bids
          assert(a.state.donations.contains(donation))
          assert(bids.contains(bid))
          assert(a.state.bids.find(_ == bid).get.donation == donation)
        }

        assert(Network.getAgents.size == 3)
      }

      "bid_acceptance" in {
        for (a <- getAgents) {
//          println(s"bid_acceptance ${a}")
          val bids = a.state.bids
          assert(bids.contains(bid))
          assert(a.state.donations.contains(donation))
        }
        Thread.sleep(6000)
        waitClearQueue()
        val coinState = aps(0).agentInLastState.state.coins
        for (a <- getAgents) {
          assert(!a.state.bids.contains(bid))
          assert(!a.state.donations.contains(donation))
          assert(!a.state.bidsPendingSettle.contains(bid))
          a.state.chains.transactions should have size(1)
          a.state.coins shouldBe coinState
        }

        val newBalances = aps map {_.agentInLastState} map Accounting.getSelfBalance
        newBalances(0) - balances(0) shouldBe 1
        newBalances(1) - balances(1) shouldBe (-1)
        newBalances(2) - balances(2) shouldBe 0
      }
    }

  FastTestScheduler.stop()

  def getAgents: Iterable[Agent] = {
    Network.getAgents.map(_.agentInLastState)
  }

  object FastTestScheduler extends Scheduler {
    override val cycleTime = 6000
  }
}