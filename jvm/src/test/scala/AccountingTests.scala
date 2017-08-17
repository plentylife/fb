import java.util.Date

import plenty.agent.AgentManager
import plenty.agent.model.Agent
import plenty.network.{MintPress, Network}
import plenty.state.StateManager
import plenty.state.model.{Coin, Node, State}
import utest._
import plenty.agent.Accounting

/**
  * Minting and distributing coins, getting balances, preventing illegial actions
  */
class AccountingTests extends TestSuite {

  val as = (0 until 4).map(i => Agent(s"a$i", State()))
  val ns = as map AgentManager.agentAsNode
  for (a <- as) Network.registerAgent(a, MockSendInterface)
  val aps = Network.getAgents.toSeq

  var originalCoins = Set[Coin]()

  val tests = this {
    'minting {
      'on_first_run {
        distributeCoins
        waitClearQueue

        for (ap <- Network.getAgents; n <- ns) {
          val balance = Accounting.getBalance(n)(ap.getAgentInLastKnownState)
          balance ==> TestMintPress.coinsPerPeriod
        }
        val now = new Date().getTime
        for (ap <- Network.getAgents) {
          assert(ap.getAgentInLastKnownState.state.coins.nonEmpty)
          ap.getAgentInLastKnownState.state.coins.forall(_.deathTime <= now + TestMintPress.period)
        }
        originalCoins = aps(0).getAgentInLastKnownState.state.coins
//        println(s"setting original coins $originalCoins")
      }

      'second_immediate_run {
        distributeCoins
        waitClearQueue

        for (ap <- Network.getAgents; n <- ns) {
          val balance = Accounting.getBalance(n)(ap.getAgentInLastKnownState)
          balance ==> TestMintPress.coinsPerPeriod
        }
        aps(1).getAgentInLastKnownState.state.coins ==> originalCoins
      }

      'third_later_run {
        Thread.sleep(TestMintPress.period)
        distributeCoins
        waitClearQueue

        println(s"original coins $originalCoins")
        println(s"third run coins ${as(1).state.coins}")

        for (ap <- Network.getAgents; n <- ns) {
          val balance = Accounting.getBalance(n)(ap.getAgentInLastKnownState)
          balance ==> TestMintPress.coinsPerPeriod
        }
        assert(aps(1).getAgentInLastKnownState.state.coins != originalCoins)
      }
    }

    'accounting {
      'expired {
        distributeCoins
        waitClearQueue

        for (ap <- Network.getAgents; n <- ns) {
          val balance = Accounting.getBalance(n)(ap.getAgentInLastKnownState)
          balance ==> TestMintPress.coinsPerPeriod
        }

        Thread.sleep(TestMintPress.period + 1)
        for (ap <- Network.getAgents; n <- ns) {
          val balance = Accounting.getBalance(n)(ap.getAgentInLastKnownState)
          balance ==> 0
        }

        val agentWithClearedCoins = Accounting.clearDeadCoins(aps.head.getAgentInLastKnownState)
        assert(agentWithClearedCoins.state.coins.isEmpty)
      }
    }
  }

  def distributeCoins = {
    TestMintPress.distributeCoins(Network.getAgents.map(_.getAgentInLastKnownState))
  }

  def waitClearQueue = {
    println("waiting on message queue")
    println(s"non-completes: ${Network.nonCompletes.mkString(" ")}")
    while (Network.nonCompletes.nonEmpty) {
      Thread.sleep(300)
      println(s"non-completes (loop): ${Network.nonCompletes.mkString(" ")}")
    }
  }
}

object TestMintPress extends MintPress {
  override val coinsPerPeriod: Int = 1
  override val period = 3000
}
