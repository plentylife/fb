import BiddingTests.{assert, bidFirst, bidProper, donation}
import TestUtilities._
import plenty.agent.model.Agent
import plenty.agent.{Accounting, ActionLogic, AgentManager, AgentPointer}
import plenty.network.{BidAction, _}
import plenty.state.StateManager
import plenty.state.model._
import utest._

/**
  * Saving state, modifying state by agents
  */
object TransactionTests extends TestSuite {

  val a = (0 until 4).map(i => Agent(s"a$i", State()))
  var ap = Seq[AgentPointer]()
  val n = a map AgentManager.agentAsNode

  val donation = StateManager.createDonation("d-title", "d-desc", Seq(), AgentManager.agentAsNode(a(0)))
  var bidFirst = StateManager.createBid(donation, amount = 2, by = n(1))
  var bidSecond = StateManager.createBid(donation, amount = 3, by = n(3))

  var balances = Seq[Int]()

  val tests = this {
    'one_bid_not_enough_coins_receiving_end {
      'setting_up {
        ap = a map { _a => Network.registerAgent(_a, InternalSendInterface) }
        a foreach MintPress.distributeCoinsToNewAgent
        waitClearQueue
        balances = ap map {
          _.getAgentInLastKnownState
        } map {
          Accounting.getSelfBalance
        }

        Network.notifyAllAgents(donation, DonateAction, n(0))
        val msg1 = Message.createMessage(bidFirst.by, donation.by, BidAction, bidFirst)

        Network.send(msg1)
        waitClearQueue
      }

      'taking_bids {
        InternalSendInterface.clearLog

        var s0 = ap(0).getAgentInLastKnownState.state
        s0 = s0.copy(coins = Set())
        ap(0).set(a(0).copy(state = s0))

        // the receiving end believes that the sending end does not have enough coins
        ActionLogic.takeBids(ap(0).getAgentInLastKnownState)

        waitClearQueue

        val assertTransactionValid = InternalSendInterface.log map {_.payload} collect {
          case r: RejectedTransaction ⇒
            val t = r.transaction
            val liveCoins = Accounting.filterDeadCoins(t.coins)
            // all coins belong to the bidder and the amount matches the amount agreed upon
            (liveCoins map {_.belongsTo == n(1)} forall {v⇒v}) && (liveCoins.size == bidFirst.amount)
        }
        val assertNoCoinsMinted = InternalSendInterface.log map {_.payloadId == ActionIdentifiers.COINS_MINTED}

        assert(assertTransactionValid.nonEmpty && assertTransactionValid.forall{v⇒v})
        assert(!assertNoCoinsMinted.contains(true))
//        for (a <- getAgents) {
//          val bids = a.state.bids
//          assert(a.state.donations.contains(donation))
//          assert(!bids.contains(bidFirst))
//          assert(bids.contains(bidProper))
//        }

      }
    }


  }

  object InternalSendInterface extends SendInterface {
    var log: Seq[Message[_]] = Seq[Message[_]]()

    def clearLog = log = Seq()

    override def send(msg: Message[_]): Unit = synchronized {
      log :+= msg

      msg.payload match {
        case rejection: Rejection ⇒
          println(s"rejection reason: ${rejection.reason} ${rejection.getClass}")
        case _ ⇒
      }

      Network.receive(msg)

    }
  }
}

