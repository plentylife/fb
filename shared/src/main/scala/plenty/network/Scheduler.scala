package plenty.network

import plenty.agent.{Accounting, AgentManager}
import plenty.agent.model.Agent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

/**
  * Runs CRON type tasks
  */
trait Scheduler {

  protected val cycleTime = 2 * 60 * 60 * 1000

  private var shutdown = false

  def start() = {
    Future {
      println("Scheduler started")
      while (!shutdown) {
        Thread.sleep(cycleTime)
        Future(execute())
      }
    }
  }

  def stop() = {
    println("Scheduler stopped")
    shutdown = true
  }

  def execute() = {
    Network.getAgents.foreach(ap => {
      val p = Promise[Agent]()
      ap.getAgentToModify(p)
      p.future.map(agent => {
        AgentManager.acceptBids(agent)
        val a = Accounting.clearDeadCoins(agent)
        ap.set(a)
      })
    })
  }
}

object Scheduler extends Scheduler
