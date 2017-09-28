package fb

import java.io.File
import java.util.Date

import com.softwaremill.quicklens._
import org.scalatest.{FreeSpec, Matchers}
import plenty.TestUtilities._
import plenty.agent.logic.AgentManager
import plenty.agent.{Accounting, AgentPointer}
import plenty.executionContext
import plenty.network.{MintPress, Network}
import plenty.state.StateIO
import plenty.state.model.{Coin, DemurageTransaction, Node, TransactionType}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.postfixOps

class AccountingFbTests extends FreeSpec with Matchers {

  "Account creation" - {
    val ns = Seq(Node("test1"), Node("test2"))
    var aps = Seq[AgentPointer]()
    var originalCoins = Seq[Set[Coin]]()

    val deleteUsers = true
    if (deleteUsers) {
      ns.foreach { n ⇒
        val f = new File(s"./data-stores/current/${n.id}.plenty")
        if (f.exists()) f.delete()
      }
      val f = new File(s"./data-stores/current/facebook_agent.plenty")
      if (f.exists()) f.delete()
    }

    "old coins should be remembered" in {
      if (!deleteUsers) {
        Network.clear
        StateIO.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
        aps = ns.collect { case n ⇒ Network.getAgents.find(_.node == n) } flatten;

        aps.foreach(ap ⇒ {
          ns foreach { n ⇒
            Accounting.getBalance(n)(ap) should not be 0
          }
        })
      }
    }

    "new user coins should belong to that user" in {
      Network.clear
      StateIO.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
      aps = ns.collect { case n ⇒ Network.getAgents.find(_.node == n) } flatten;
      FbAgent.load()
      val newAgent = Await.result(Utility.createAgent(ns(0)), Duration.Inf)
      waitClearQueue()

      Accounting.getOwnCoins(newAgent) should have size (7)
      Accounting.getBalance(ns(0))(FbAgent.pointer.agentInLastState) shouldBe 7
      Accounting.getBalance(FbAgent.pointer.node)(newAgent) shouldBe 0
      Accounting.getBalance(FbAgent.pointer.node)(FbAgent.pointer) shouldBe 0
      aps = aps :+ newAgent
      originalCoins = originalCoins :+ Accounting.getOwnCoins(newAgent)
    }

    "both users should have same amounts of coins after another user is created" in {
      Network.clear
      StateIO.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
      aps = ns.collect { case n ⇒ Network.getAgents.find(_.node == n) } flatten;
      FbAgent.load()

      div("1")
      val newAgent = Await.result(Utility.createAgent(ns(1)), Duration.Inf)
      waitClearQueue()

      Accounting.getOwnCoins(newAgent) should have size (7)
      Accounting.getBalance(ns(0))(FbAgent.pointer.agentInLastState) shouldBe 7
      Accounting.getBalance(ns(0))(newAgent) shouldBe 7
      Accounting.getBalance(ns(1))(aps(0)) shouldBe 7
      Accounting.getBalance(ns(1))(FbAgent.pointer) shouldBe 7

      Accounting.getOwnCoins(aps(0)) shouldBe originalCoins(0)
      Accounting.getCoins(ns(0), FbAgent.pointer) shouldBe originalCoins(0)
      Accounting.getCoins(ns(0), newAgent) shouldBe originalCoins(0)

      Accounting.getBalance(FbAgent.pointer.node)(newAgent) shouldBe 0
      Accounting.getBalance(FbAgent.pointer.node)(FbAgent.pointer) shouldBe 0
      Accounting.getBalance(FbAgent.pointer.node)(aps(0)) shouldBe 0

      aps = aps :+ newAgent
      originalCoins = originalCoins :+ Accounting.getOwnCoins(newAgent)
    }

    "agent should be loaded even if not present in the network " in {
      Network.clear
      ns foreach { n ⇒
        ReceiverFlow.getAgent(n.id).get
      }
    }


    "coins belonging to users should not be distributed again" in {
      div("2")
      Network.clear
      StateIO.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
      aps = ns.collect { case n ⇒ Network.getAgents.find(_.node == n) } flatten;
      FbAgent.load()

      val usedCoins = aps flatMap { a ⇒ Accounting.getOwnCoins(a) } toSet;
      val rightCountUserCoins = 7 * aps.size
      usedCoins should have size (rightCountUserCoins)

      val minted: Set[Coin] = MintPress.fillCoinSet(FbAgent.lastState.coins, FbAgent.node)
      (minted intersect usedCoins) shouldBe empty
      minted should not be empty
    }

    "Demurrage" - {
      "After a short time" - {
        "Users should still have 7 coins" in {
          Network.clear
          StateIO.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
          aps = ns.collect { case n ⇒ Network.getAgents.find(_.node == n) } flatten;
          FbAgent.load()

          div("3")

          Network.getAgents foreach { a ⇒
            AgentManager.applyDemurrage(a)
          }
          waitClearQueue()

          Accounting.getBalance(ns(0))(FbAgent.pointer.agentInLastState) shouldBe 7
          Accounting.getBalance(ns(0))(aps(1)) shouldBe 7
          Accounting.getBalance(ns(1))(aps(0)) shouldBe 7
          Accounting.getBalance(ns(1))(FbAgent.pointer) shouldBe 7

          Accounting.getOwnCoins(aps(0)) shouldBe originalCoins(0)
          Accounting.getCoins(ns(0), FbAgent.pointer) shouldBe originalCoins(0)
          Accounting.getCoins(ns(0), aps(1)) shouldBe originalCoins(0)

          Accounting.getOwnCoins(aps(1)) shouldBe originalCoins(1)
          Accounting.getCoins(ns(1), FbAgent.pointer) shouldBe originalCoins(1)
          Accounting.getCoins(ns(1), aps(0)) shouldBe originalCoins(1)

        }
      }

      "After a long time" - {
        "users should have less coins" in {
          Network.clear
          StateIO.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
          aps = ns.collect { case n ⇒ Network.getAgents.find(_.node == n) } flatten;
          FbAgent.load()

          div("4")

          val now = new Date().getTime
          def fakeT(from: Node) =
            DemurageTransaction("fake", now - plenty.daysToMillis(4), Set(), from, ns(0))

          // setting a fake demurrage transaction to yesterday
          Network.getAgents foreach { ap ⇒
            Await.ready(ap.getAgentToModify().map({ a ⇒
              val upd = a.modify(_.state.chains.transactions).using(_ :+ fakeT(a.node))
              ap.set(upd)
            }), Duration.Inf)
          }
          waitClearQueue()

          Network.getAgents foreach { a ⇒
            AgentManager.applyDemurrage(a)
          }
          waitClearQueue()

          val bal = 6

          Accounting.getOwnCoins(aps(0)) intersect originalCoins(0) should have size (bal)
          Accounting.getCoins(ns(0), FbAgent.pointer) intersect originalCoins(0) should have size (bal)
          Accounting.getCoins(ns(0), aps(1)) intersect originalCoins(0) should have size (bal)

          Accounting.getOwnCoins(aps(1)) intersect originalCoins(1) should have size (bal)
          Accounting.getCoins(ns(1), FbAgent.pointer) intersect originalCoins(1) should have size (bal)
          Accounting.getCoins(ns(1), aps(0)) intersect originalCoins(1) should have size (bal)

          val fbPerspect = Accounting.getCoins(ns(0), FbAgent.pointer) ++ Accounting.getCoins(ns(1), FbAgent.pointer)
          fbPerspect should have size (14)
          fbPerspect intersect originalCoins.flatten.toSet should have size 14
        }
      }
    }

    "Transaction tracking" - {
      "Demurrage transactions shoud be present for all" in {
        Network.clear
        StateIO.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
        aps = ns.collect { case n ⇒ Network.getAgents.find(_.node == n) } flatten;
        FbAgent.load()

        div("5")

        (aps :+ FbAgent.pointer) foreach { ap ⇒
          val ts = ap.agentInLastState.state.chains.transactions.filterNot(_.id == "fake")
          ts should have size (2)
          ns foreach { n ⇒
            ts.count(t ⇒ t.transactionType == TransactionType.DEMURAGE && t.from == n && t.to != n) shouldBe 1
          }
        }
      }
    }

    "Agent states" - {
      "Should not contain fb_agent but all others" in {
        Network.clear
        StateIO.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
        aps = ns.collect { case n ⇒ Network.getAgents.find(_.node == n) } flatten;
        FbAgent.load()

        div("6")

        (aps :+ FbAgent.pointer) foreach { ap ⇒
          ap.agentInLastState.state.nodes should not contain FbAgent.node
          ap.agentInLastState.state.nodes should not contain all(ns.toSet - ap.node)
        }
      }
    }
  }

}
