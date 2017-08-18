import TestUtilities._
import plenty.agent.model.Agent
import plenty.agent.{Accounting, AgentManager, AgentPointer}
import plenty.network.{BidAction, _}
import plenty.state.StateManager
import plenty.state.model.{RejectedBid, State}
import utest._

/**
  * Saving state, modifying state by agents
  */
object BiddingTests extends TestSuite {

  val a = (0 until 4).map(i => Agent(s"a$i", State()))
  var ap = Seq[AgentPointer]()
  val n = a map AgentManager.agentAsNode

  val donation = StateManager.createDonation("d-title", "d-desc", Seq(), AgentManager.agentAsNode(a(0)))
  var bidFirst = StateManager.createBid(donation, amount = 2, by = n(1))
  var bidUnder = StateManager.createBid(donation, amount = 1, by = n(2))
  var bidOver = StateManager.createBid(donation, amount = 3, by = n(3))
  var bidOverWallet = StateManager.createBid(donation, amount = 30, by = n(3))
  var bidSelf = StateManager.createBid(donation, amount = 3, by = n(0))


  var balances = Seq[Int]()

  val tests = this {
    'bidding_wars {

      'setting_up {
        ap = a map { _a => Network.registerAgent(_a, InternalSendInterface) }
        a foreach MintPress.distributeCoinsToNewAgent
        waitClearQueue
        balances = ap map {
          _.getAgentInLastKnownState
        } map {
          Accounting.getSelfBalance
        }

        Network.notifyAllAgents(donation, DonateAction, from = AgentManager.agentAsNode(a(0)))
        waitClearQueue

        val msg = Message.createMessage(bidFirst.by, donation.by, BidAction, bidFirst)
        Network.send(msg)
        waitClearQueue
      }

      'bidding_on_own_donation {
        val msg = Message.createMessage(bidSelf.by, donation.by, BidAction, bidSelf)

        Network.send(msg)
        waitClearQueue

        for (a <- getAgents) {
          val bids = a.state.bids
          assert(a.state.donations.contains(donation))
          assert(bids.contains(bidFirst))
          assert(!bids.contains(bidSelf))
        }

        val payloads = InternalSendInterface.log map { m => m.payload }
        val rejected = payloads collect {
          case rej: RejectedBid => rej.bid == bidSelf
        }
        assert(rejected contains true)
      }

      'bidding_under {
        println("=== bidding under")
        val msg = Message.createMessage(bidUnder.by, donation.by, BidAction, bidUnder)

        Network.send(msg)
        waitClearQueue

        for (a <- getAgents) {
          val bids = a.state.bids
          assert(a.state.donations.contains(donation))
          assert(bids.contains(bidFirst))
          assert(!bids.contains(bidUnder))
        }

        val payloads = InternalSendInterface.log map { m => m.payload }
        val rejected = payloads collect {
          case rej: RejectedBid => rej.bid == bidUnder
        }
        assert(rejected contains true)
      }

      'bidding_over_wallet_size {
        println("=== bidding over wallet size")
        val msg = Message.createMessage(bidOverWallet.by, donation.by, BidAction, bidOverWallet)

        Network.send(msg)
        waitClearQueue

        for (a <- getAgents) {
          val bids = a.state.bids
          assert(a.state.donations.contains(donation))
          assert(bids.contains(bidFirst))
          assert(!bids.contains(bidOverWallet))
        }

        val payloads = InternalSendInterface.log map { m => m.payload }
        val rejected = payloads collect {
          case rej: RejectedBid => rej.bid == bidOverWallet
        }
        assert(rejected contains true)
      }
    }
  }

  object InternalSendInterface extends SendInterface {
    var log = Seq[Message[_]]()
    override def send(msg: Message[_]): Unit = synchronized {
      log :+= msg
      Network.receive(msg)
    }
  }
}

