import java.util.Date

import TestUtilities._
import org.scalatest.{FreeSpec, Matchers}
import plenty.agent.{Accounting, AgentPointer}
import plenty.agent.model.Agent
import plenty.network.{ActionIdentifiers, MintPress, Network}
import plenty.state.StateManager
import plenty.state.model.{Coin, Node, State}

import scala.collection.immutable
import scala.language.postfixOps

class DemurageTests extends FreeSpec with Matchers {
  "Demurrage" - {
    val numNodes = 4
    val now = new Date().getTime
    val ns = (0 until numNodes) map { i ⇒ Node(i.toString) }
    val cs = MintPress.fillCoinSet(Set(), ns(0))
    val csg = cs.grouped(100).toSeq
    val s = State(nodes = ns.toSet, coins = cs)
    val as = ns.map(n => Agent(n, s))
    val ap = as map { a ⇒ Network.registerAgent(a, MockSendReceiveInterface) }

    // giving all coins from 0 to 1, so that 1 has transactions of spending
    val csAll = csg.take(numNodes - 2).flatten
    val t = StateManager.createTransaction(csAll.toSet, from = ns(0), to = ns(1))
    Network.notifyAllAgents(t, ActionIdentifiers.TRANSACTION, ns(0))
    waitClearQueue()

    ns.tail.tail zip csg foreach { case (n, c) ⇒
      Stream(ns(1)) map { from ⇒ from → StateManager.createTransaction(c, from, n) } foreach { case (f, p) ⇒
        Network.notifyAllAgents(p, ActionIdentifiers.TRANSACTION, f)
      }
    }
    waitClearQueue()

    "demurrage rates should be high for agents who have not spent" in {
      val nonSpenders = ap.tail.tail map {_.agentInLastState}
      val rates = nonSpenders map Accounting.calculateDemurageRate

      rates foreach { r ⇒ assert(r == 0.05) }
    }

    "demurrage rate should be low for agents who have spent" in {
      assert(Accounting.calculateDemurageRate(ap(1).agentInLastState) <= 0.01)
    }

    "demurrage should be zero" in {
      // because a1 has no funds left
      val d1 = Accounting.calculateDemurage(ap(1), now + plenty.daysToMillis(1))
      d1 shouldBe 0.0
    }

    "demurrage should be non-zero" in {
      // taking non-spenders
      ap.takeRight(numNodes - 2) foreach { a ⇒
        Accounting.calculateDemurage(a, now + plenty.daysToMillis(1)) shouldBe 5.0 +- 0.1
      }
    }
  }
}

class TransactionTests extends FreeSpec {
  val numNodes = 3
  val ns: immutable.IndexedSeq[Node] = (0 until numNodes) map { i ⇒ Node(i.toString) }
  val cs: Set[Coin] = MintPress.fillCoinSet(Set(), ns(0))
  val csg: Iterator[Set[Coin]] = cs.grouped(100)
  val s = State(nodes = ns.toSet, coins = cs)
  val as: immutable.IndexedSeq[Agent] = ns.map(n => Agent(n, s))

  "Illegal transactions" - {
    Network.clear
    val ap = as map { a ⇒ Network.registerAgent(a, MockSendReceiveInterface) }

    "coins belonging to someone else should not be accepted" in {
      val t = StateManager.createTransaction(csg.next(), ns(1), to = ns(0))
      Network.notifyAllAgents(t, ActionIdentifiers.TRANSACTION, ns(1))
      waitClearQueue()

      assert(MockSendReceiveInterface.log.exists(_.payloadId == ActionIdentifiers.REJECT_TRANSACTION))
      assert(!MockSendReceiveInterface.log.exists(_.payloadId == ActionIdentifiers.ACCEPT_TRANSACTION))
    }

    "there should be no transactions in history" in {
      ap map {_.agentInLastState} map {_.state.chains.transactions} foreach { ts ⇒
        assert(ts.isEmpty)
      }
    }
  }

  "Legal transactions" - {
    Network.clear
    val ap = as map { a ⇒ Network.registerAgent(a, MockSendReceiveInterface) }

    "spreading wealth (setup) " in {
      ns.tail zip csg.toSet foreach { case (n, c) ⇒
        Stream(ns(0)) map { from ⇒ from → StateManager.createTransaction(c, from, n) } foreach { case (f, p) ⇒
          Network.notifyAllAgents(p, ActionIdentifiers.TRANSACTION, f)
        }
      }
      waitClearQueue()
    }

    s"there should only be ${numNodes - 1} transactions in history" in {
      ap map {_.agentInLastState} map {_.state.chains.transactions} foreach { ts ⇒
        assert(ts.length == numNodes - 1)
      }
    }
  }
}
