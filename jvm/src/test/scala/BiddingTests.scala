import plenty.TestUtilities._
import plenty.agent.model.Agent
import plenty.agent.{Accounting, AgentManager, AgentPointer}
import plenty.network.{BidAction, _}
import plenty.state.StateManager
import plenty.state.model.{Bid, RejectedBid, State}


/**
  * Saving state, modifying state by agents
  */
//class BiddingTests extends TestSuite {
//
//  val a = (0 until 4).map(i => Agent(s"a$i", State()))
//  var ap = Seq[AgentPointer]()
//  val n = a map AgentManager.agentAsNode
//
//  val donation = StateManager.createEmptyDonation(AgentManager.agentAsNode(a(0)))
//  var bidFirst = StateManager.createBid(donation, amount = 2, by = n(1))
//  var bidUnder = StateManager.createBid(donation, amount = 1, by = n(2))
//  var bidProper = StateManager.createBid(donation, amount = 3, by = n(3))
//  var bidOverWallet = StateManager.createBid(donation, amount = 30, by = n(3))
//  var bidSelf = StateManager.createBid(donation, amount = 3, by = n(0))
//
//
//  var balances = Seq[Int]()
//
//  val tests = this {
//    'bidding_wars {
//
//      'setting_up {
//        ap = a map { _a => Network.registerAgent(_a, InternalSendReceiveInterface$) }
//        a foreach MintPress.distributeCoinsToNewAgent
//        waitClearQueue()
//        balances = ap map {
//          _.getAgentInLastKnownState
//        } map {
//          Accounting.getSelfBalance
//        }
//      }
//
//      'bidding_on_non_existent_donation {
//        val msg = Message.createMessage(bidProper.by, donation.by, BidAction, bidProper)
//
//        Network.send(msg)
//        waitClearQueue()
//
//        for (a <- getAgents) {
//          val bids = a.state.bids
//          assert(!a.state.donations.contains(donation))
//          assert(!bids.contains(bidFirst))
//          assert(!bids.contains(bidProper))
//        }
//
//        val payloads = InternalSendReceiveInterface$.log map { m => m.payload }
//        val rejected = payloads collect {
//          case rej: RejectedBid => rej.bid == bidProper
//        }
//        assert(rejected contains true)
//      }
//
//      'setting_up_donation_and_bid {
//        Network.notifyAllAgents(donation, DonateAction, from = AgentManager.agentAsNode(a(0)))
//        waitClearQueue()
//
//        val msg = Message.createMessage(bidFirst.by, donation.by, BidAction, bidFirst)
//        Network.send(msg)
//        waitClearQueue()
//      }
//
//      'bidding_on_own_donation {
//        val msg = Message.createMessage(bidSelf.by, donation.by, BidAction, bidSelf)
//
//        Network.send(msg)
//        waitClearQueue()
//
//        for (a <- getAgents) {
//          val bids = a.state.bids
//          assert(a.state.donations.contains(donation))
//          assert(bids.contains(bidFirst))
//          assert(!bids.contains(bidSelf))
//        }
//
//        val payloads = InternalSendReceiveInterface$.log map { m => m.payload }
//        val rejected = payloads collect {
//          case rej: RejectedBid => rej.bid == bidSelf
//        }
//        assert(rejected contains true)
//      }
//
//      'bidding_under {
//        println("=== bidding under")
//        val msg = Message.createMessage(bidUnder.by, donation.by, BidAction, bidUnder)
//
//        Network.send(msg)
//        waitClearQueue()
//
//        for (a <- getAgents) {
//          val bids = a.state.bids
//          assert(a.state.donations.contains(donation))
//          assert(bids.contains(bidFirst))
//          assert(!bids.contains(bidUnder))
//        }
//
//        val payloads = InternalSendReceiveInterface$.log map { m => m.payload }
//        val rejected = payloads collect {
//          case rej: RejectedBid => rej.bid == bidUnder
//        }
//        assert(rejected contains true)
//      }
//
//      'bidding_over_wallet_size {
//        println("=== bidding over wallet size")
//        val msg = Message.createMessage(bidOverWallet.by, donation.by, BidAction, bidOverWallet)
//
//        Network.send(msg)
//        waitClearQueue()
//
//        for (a <- getAgents) {
//          val bids = a.state.bids
//          assert(a.state.donations.contains(donation))
//          assert(bids.contains(bidFirst))
//          assert(!bids.contains(bidOverWallet))
//        }
//
//        val payloads = InternalSendReceiveInterface$.log map { m => m.payload }
//        val rejected = payloads collect {
//          case rej: RejectedBid => rej.bid == bidOverWallet
//        }
//        assert(rejected contains true)
//      }
//
//      'bidding_proper {
//        val msg = Message.createMessage(bidProper.by, donation.by, BidAction, bidProper)
//
//        Network.send(msg)
//        waitClearQueue()
//
//        for (a <- getAgents) {
//          val bids = a.state.bids
//          assert(a.state.donations.contains(donation))
//          assert(bids.contains(bidFirst))
//          assert(bids.contains(bidProper))
//        }
//
//        val payloads = InternalSendReceiveInterface$.log map { m => m -> m.payload }
//        val rejected = payloads collect {
//          case (m: Message[_], b: Bid) => b == bidProper && m.payloadId == ActionIdentifiers.ACCEPT_BID_ACTION && m
//            .to == bidProper.by
//        }
//        assert(rejected contains true)
//      }
//    }
//  }
//
//  object InternalSendReceiveInterface$ extends SendReceiveInterface {
//    var log = Seq[Message[_]]()
//    override def send(msg: Message[_]): Unit = synchronized {
//      log :+= msg
//      Network.receive(msg)
//    }
//  }
//}

