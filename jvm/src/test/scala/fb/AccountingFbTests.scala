package fb

import java.io.File
import java.util.Date

import com.softwaremill.quicklens._
import org.scalatest.{FreeSpec, Matchers}
import plenty.TestUtilities._
import plenty.agent.model.Agent
import plenty.agent.{Accounting, ActionLogic, AgentPointer}
import plenty.executionContext
import plenty.network.{MintPress, Network}
import plenty.state.StateManager
import plenty.state.model.{Coin, DemurageTransaction, Node}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.language.postfixOps

class AccountingFbTests extends FreeSpec with Matchers {

  "Account creation" - {
    val ns = Seq(Node("test1"), Node("test2"))
    var aps = Seq[AgentPointer]()
    var originalCoins = Seq[Set[Coin]]()

    ns.foreach {n ⇒
      val f = new File(s"./data-stores/current/${n.id}.plenty")
      if (f.exists()) f.delete()
    }
    // fixme do not delete facebook, this should all still work even with accumilating tests
    val f = new File(s"./data-stores/current/facebook_agent.plenty")
    if (f.exists()) f.delete()


    "new user coins should belong to that user" in {
      val newAgent = Await.result(
        FbAgent.load()
          Utility.createAgent(ns(0))
        Duration.Inf)
      waitClearQueue()

      newAgent.state.coins should have size (2023)
      Accounting.getOwnCoins(newAgent) should have size (7)
      Accounting.getBalance(ns(0))(FbAgent.pointer.agentInLastState) shouldBe 7
      FbAgent.pointer.agentInLastState.state.coins should have size (2023)
      aps = aps :+ newAgent
      originalCoins = originalCoins :+ Accounting.getOwnCoins(newAgent)
    }

    "both users should have same amounts of coins after another user is created" in {
      Network.clear
      StateManager.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
      aps = ns.collect {case n ⇒ Network.getAgents.find(_.node == n) } flatten;
      FbAgent.load()

      div("1")
      val newAgent = Await.result(Utility.createAgent(ns(1)), Duration.Inf)
      waitClearQueue()


      val coinsNotBelongingToFbAgentN1 = aps(0).agentInLastState.state.coins.filterNot(_.belongsTo == FbAgent.node)
      val coinsNotBelongingToFbAgentFB = FbAgent.pointer.state.coins.filterNot(_.belongsTo == FbAgent.node)
      Accounting.getOwnCoins(newAgent) should have size (7)
      newAgent.state.coins should have size (2023)
      Accounting.getBalance(ns(0))(FbAgent.pointer.agentInLastState) shouldBe 7
      Accounting.getBalance(ns(0))(newAgent) shouldBe 7
      Accounting.getBalance(ns(1))(aps(0)) shouldBe 7
      Accounting.getBalance(ns(1))(FbAgent.pointer) shouldBe 7

      Accounting.getOwnCoins(aps(0)) shouldBe originalCoins(0)
      Accounting.getCoins(ns(0), FbAgent.pointer) shouldBe originalCoins(0)
      Accounting.getCoins(ns(0), newAgent) shouldBe originalCoins(0)

      aps = aps :+ newAgent
      originalCoins = originalCoins :+ Accounting.getOwnCoins(newAgent)
    }

    "coins belonging to users should not be distributed again" in {
      div("2")
      Network.clear
      StateManager.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }

      val usedCoins = aps flatMap { a ⇒ Accounting.getOwnCoins(a) } toSet;
      val rightCountUserCoins = 7 * aps.size
      usedCoins should have size (rightCountUserCoins)

      val minted: Set[Coin] = MintPress.fillCoinSet(FbAgent.lastState.coins, FbAgent.node)
      (minted intersect usedCoins) shouldBe empty
      minted should have size (0)
    }

    "Demurrage" - {
      "After a short time" - {
        "Users should still have 7 coins" in {
          div("3")

          Network.clear
          StateManager.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }

          Network.getAgents foreach { a ⇒
            ActionLogic.applyDemurage(a)
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

      "After a long time" in {
        div("4")
        Network.clear
        StateManager.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }

        val now = new Date().getTime
        val fakeT = DemurageTransaction("fake", now - plenty.daysToMillis(1), Set(), FbAgent.node, ns(0))
        val prom = Promise[Agent]()
        FbAgent.pointer.getAgentToModify(prom)

        Await.ready(prom.future.map { a ⇒
          val upd = a.modify(_.state.chains.transactions)
            .using(_ :+ fakeT)
        }, Duration.Inf)

        Network.getAgents foreach { a ⇒
          ActionLogic.applyDemurage(a)
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
  }

}
