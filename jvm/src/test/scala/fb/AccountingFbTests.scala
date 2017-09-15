package fb

import org.scalatest.{FreeSpec, Matchers}
import plenty.TestUtilities._
import plenty.agent.{Accounting, ActionLogic, AgentPointer}
import plenty.state.model.Node

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import plenty.executionContext
import plenty.network.Network

class AccountingFbTests extends FreeSpec with Matchers {

  "Account creation" - {
    val ns = Seq(Node("test1"), Node("test2"))
    var aps = Seq[AgentPointer]()

    "new user coins should belong to that user" in {


      val newAgent = Await.result(
        FbAgent.load() flatMap {_ ⇒ Utility.createAgent(ns(0))},
        Duration.Inf)
      waitClearQueue()

      newAgent.state.coins should have size (2023)
      Accounting.getOwnCoins(newAgent) should have size (7)
      Accounting.getBalance(ns(0))(FbAgent.pointer.agentInLastState) shouldBe 7
      FbAgent.pointer.agentInLastState.state.coins should have size (2023)
      aps = aps :+ newAgent
    }

    "both users should have same amounts of coins after another user is created" in {
      div

      val newAgent = Await.result(Utility.createAgent(ns(1)), Duration.Inf)
      waitClearQueue()

      Accounting.getOwnCoins(newAgent) should have size (7)
      newAgent.state.coins should have size (2023)
      Accounting.getBalance(ns(0))(FbAgent.pointer.agentInLastState) shouldBe 7
      Accounting.getBalance(ns(0))(newAgent) shouldBe 7
      Accounting.getBalance(ns(1))(aps(0)) shouldBe 7
      Accounting.getBalance(ns(1))(FbAgent.pointer) shouldBe 7
      aps = aps :+ newAgent
    }

    "Demurrage" - {
      "After a short time" - {
        "Users should still have 7 coins" in {
         div

          Network.getAgents foreach {a ⇒
            ActionLogic.applyDemurage(a)
          }
          waitClearQueue()

          Accounting.getBalance(ns(0))(FbAgent.pointer.agentInLastState) shouldBe 7
          Accounting.getBalance(ns(0))(aps(1)) shouldBe 7
          Accounting.getBalance(ns(1))(aps(0)) shouldBe 7
          Accounting.getBalance(ns(1))(FbAgent.pointer) shouldBe 7

        }
      }
    }
  }

}
