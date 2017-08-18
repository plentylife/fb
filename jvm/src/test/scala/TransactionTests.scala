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

  import S._

  val tests = this {
    'one_bid {
      'not_enough_coins_receiving_end {
        'setting_up {
          reset()

          val msg1 = Message.createMessage(S.bidFirst.by, S.donation.by, BidAction, S.bidFirst)
          Network.send(msg1)
          waitClearQueue()
        }

        'taking_bids {
          InternalSendInterface.clearLog

          var s0 = ap(0).getAgentInLastKnownState.state
          s0 = s0.copy(coins = Set())
          ap(0).set(a(0).copy(state = s0))

          // the receiving end believes that the sending end does not have enough coins
          ActionLogic.takeBids(ap(0).getAgentInLastKnownState)

          waitClearQueue()

          val assertTransactionValid = InternalSendInterface.log map {_.payload} collect {
            case r: RejectedTransaction ⇒
              val t = r.transaction
              val liveCoins = Accounting.filterDeadCoins(t.coins)
              // all coins belong to the bidder and the amount matches the amount agreed upon
              (liveCoins map {_.belongsTo == n(1)} forall { v ⇒ v }) && (liveCoins.size == bidFirst.amount)
          }
          val assertNoCoinsMinted = InternalSendInterface.log map {_.payloadId == ActionIdentifiers.COINS_MINTED}

          assert(assertTransactionValid.nonEmpty && assertTransactionValid.forall { v ⇒ v })
          assert(!assertNoCoinsMinted.contains(true))
          for (a <- getAgents) {
            val bids = a.state.bids
            assert(a.state.donations.contains(donation))
            assert(!bids.contains(bidFirst))
            assert(!a.state.nonSettledBids.contains(bidFirst))
          }
        }
      }

      'not_enough_coins_sending_end {
        'setting_up {
          reset()

          //        val netStates = Network.getAgents map {_.getAgentInLastKnownState.state}
          //        val selfStates = ap map {pointer ⇒ pointer.getAgentInLastKnownState.state}
          //        val stateDiff =  netStates diff selfStates.toSet
          //        println()
          //        println(s"pointer diff ${Network.getAgents diff ap.toSet}")
          //        println(s"state diff ${stateDiff.size}")
          //        println(s"agents in ap ${ap.size} and net ${Network.getAgents.size}")
          //        assert(stateDiff.isEmpty)

          val msg1 = Message.createMessage(bidFirst.by, donation.by, BidAction, S.bidFirst)
          Network.send(msg1)
          waitClearQueue()

          for (a <- getAgents) {
            val bids = a.state.bids
            assert(a.state.donations.contains(donation))
            assert(bids.contains(bidFirst))
          }
        }

        'taking_bids {
          InternalSendInterface.clearLog

          var s1 = ap(1).getAgentInLastKnownState.state
          s1 = s1.copy(coins = Set())
          ap(1).set(a(1).copy(state = s1))

          // the sender realizes that they don't have enough coins, but not the receiver
          ActionLogic.takeBids(ap(0).getAgentInLastKnownState)

          waitClearQueue()

          val assertNoSettles = InternalSendInterface.log map {_.payloadId} collect {
            case pid if pid == ActionIdentifiers.SETTLE_BID_ACTION ⇒ true
          }
          val assertRetractions = InternalSendInterface.log map {_.payloadId} collect {
            case pid if pid == ActionIdentifiers.RETRACT_BID_ACTION ⇒ true
          }
          val assertNoCoinsMinted = InternalSendInterface.log map {_.payloadId == ActionIdentifiers.COINS_MINTED}

          assert(assertNoSettles.isEmpty)
          assert(assertRetractions.nonEmpty)
          assert(!assertNoCoinsMinted.contains(true))
          for (a <- getAgents) {
            val bids = a.state.bids
            assert(a.state.donations.contains(donation))
            assert(!bids.contains(bidFirst))
            assert(!a.state.nonSettledBids.contains(bidFirst))
          }
        }
      }
    }

    'two_bids {
      'not_enough_coins_receiving_end {
        'setting_up {
          reset()

          val msg1 = Message.createMessage(S.bidFirst.by, S.donation.by, BidAction, S.bidFirst)
          Network.send(msg1)
          Network.send(Message.createMessage(S.bidFirst.by, S.donation.by, BidAction, S.bidSecond))
          waitClearQueue()
        }

        'taking_bids {

          InternalSendInterface.clearLog

          var s0 = ap(0).getAgentInLastKnownState.state
          s0 = s0.copy(coins = s0.coins.filterNot(_.belongsTo == n(1)))
          ap(0).set(a(0).copy(state = s0))

          // the receiving end believes that the sending end does not have enough coins
          ActionLogic.takeBids(ap(0).getAgentInLastKnownState)

          waitClearQueue(true)

//          val assertTransactionValid = InternalSendInterface.log map {m ⇒ (m.payload, m.payloadId)} collect {
//            case (t: Transaction, pid) if pid == ActionIdentifiers.APPROVE_SETTLE_BID_ACTION ⇒
//              val liveCoins = Accounting.filterDeadCoins(t.coins)
//              // all coins belong to the bidder and the amount matches the amount agreed upon
//              val byCond = t.from == bidSecond.by
//              val belongToCond = liveCoins map {_.belongsTo == n(2)} forall { v ⇒ v }
//              val amountCond = liveCoins.size == bidSecond.amount
//              val cond = belongToCond && amountCond && byCond
//              println(s"t from node ${t.from.id} belong $belongToCond amount $amountCond bycond ${byCond}")
//              cond
//          }
          val assertCoinsMinted = InternalSendInterface.log collect {
            case m: Message[_] if m.payloadId == ActionIdentifiers.COINS_MINTED ⇒
              val coins = m.payload.asInstanceOf[Set[Coin]]
              val newOwnerCond = coins forall(_.belongsTo == donation.by)
              val oldOwnerCond = coins forall(_.wrapsAround.get.belongsTo == n(2))
              val liveCoins = Accounting.filterDeadCoins(coins)
              val amountCond = liveCoins.size == bidSecond.amount
              println(s"newOwner $newOwnerCond oldOwner $oldOwnerCond amount $amountCond")
              oldOwnerCond && newOwnerCond && amountCond
          }

//          assert(assertTransactionValid.nonEmpty && assertTransactionValid.forall { v ⇒ v })
          assert(assertCoinsMinted.nonEmpty && assertCoinsMinted.forall(_ == true))
          for (a <- getAgents) {
            val bids = a.state.bids
            assert(!a.state.donations.contains(donation))
            assert(!bids.contains(bidFirst))
            assert(!a.state.nonSettledBids.contains(bidFirst))
          }
        }
      }

      'not_enough_coins_sending_end {
        'setting_up {
          reset()
          val msg1 = Message.createMessage(bidFirst.by, donation.by, BidAction, S.bidFirst)
          Network.send(msg1)
          Network.send(Message.createMessage(S.bidFirst.by, S.donation.by, BidAction, S.bidSecond))
          waitClearQueue()
        }

        'taking_bids {
          InternalSendInterface.clearLog

          var s1 = ap(1).getAgentInLastKnownState.state
          s1 = s1.copy(coins = Set())
          ap(1).set(a(1).copy(state = s1))

          // the sender realizes that they don't have enough coins, but not the receiver
          ActionLogic.takeBids(ap(0).getAgentInLastKnownState)

          waitClearQueue()

          val assertRetractions = InternalSendInterface.log map {_.payloadId} collect {
            case pid if pid == ActionIdentifiers.RETRACT_BID_ACTION ⇒ true
          }
          val assertTransactionValid = InternalSendInterface.log map {m ⇒ (m.payload, m.payloadId)} collect {
            case (t: Transaction, pid) if pid == ActionIdentifiers.APPROVE_SETTLE_BID_ACTION ⇒
              val liveCoins = Accounting.filterDeadCoins(t.coins)
              // all coins belong to the bidder and the amount matches the amount agreed upon
              (liveCoins map {_.belongsTo == n(2)} forall { v ⇒ v }) && (liveCoins.size == bidSecond.amount)
          }
          val assertCoinsMinted = InternalSendInterface.log collect {
            case m: Message[_] if m.payloadId == ActionIdentifiers.COINS_MINTED ⇒
              val coins = m.payload.asInstanceOf[Set[Coin]]
              val newOwnerCond = coins forall(_.belongsTo == donation.by)
              val oldOwnerCond = coins forall(_.wrapsAround.get.belongsTo == n(2))
              val liveCoins = Accounting.filterDeadCoins(coins)
              val amountCond = liveCoins.size == bidSecond.amount
              println(s"newOwner $newOwnerCond oldOwner $oldOwnerCond amount $amountCond")
              oldOwnerCond && newOwnerCond && amountCond
          }

          assert(assertTransactionValid.nonEmpty && assertTransactionValid.forall { v ⇒ v })
          assert(assertCoinsMinted.nonEmpty && assertCoinsMinted.forall(_ == true))
          assert(assertRetractions.nonEmpty)
          for (a <- getAgents) {
            val bids = a.state.bids
            assert(!a.state.donations.contains(donation))
            assert(!bids.contains(bidFirst))
            assert(!a.state.nonSettledBids.contains(bidFirst))
          }
        }
      }
    }
  }

}

private object S {
  val a = (0 until 4).map(i => Agent(s"a$i", State()))
  var ap = Seq[AgentPointer]()
  val n = a map AgentManager.agentAsNode

  val donation = StateManager.createDonation("d-title", "d-desc", Seq(), AgentManager.agentAsNode(a(0)))
  var bidFirst = StateManager.createBid(donation, amount = 4, by = n(1))
  var bidSecond = StateManager.createBid(donation, amount = 3, by = n(2))

  var balances = Seq[Int]()

  def reset() = {

    println("\nRESET\n")

    Network.clear
    ap = Seq()
    ap = a map { _a => Network.registerAgent(_a, InternalSendInterface) }

    assert(ap.forall(_.getAgentInLastKnownState.state.coins.isEmpty))

    a foreach MintPress.distributeCoinsToNewAgent
    waitClearQueue()

    Network.notifyAllAgents(donation, DonateAction, n(0))
    waitClearQueue()
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
