package fb

import java.util.Date

import plenty.agent.AgentManager
import plenty.agent.model.Agent
import plenty.network.{MintPress, Network}
import plenty.state.StateManager
import plenty.state.model.{Coin, Node, State}
import utest._
import plenty.agent.Accounting

import scala.util.Random

/**
  */
class MiscFbTests extends TestSuite {

  val tests = this {
    'create_agent {
      val id = "test-" + Random.nextInt(5000)
      FbAgent.load()
      val a = AgentManager.createAgent(id)

      waitClearQueue()
      FbAgent.lastState.coins.count(_.belongsTo.id == a.id) ==> 10
    }
  }

  def waitClearQueue() = {
    println("waiting on message queue")
    println(s"non-completes: ${Network.nonCompletes.mkString(" ")}")
    while (Network.nonCompletes.nonEmpty) {
      Thread.sleep(300)
      println(s"non-completes (loop): ${Network.nonCompletes.mkString(" ")}")
    }
  }
}
