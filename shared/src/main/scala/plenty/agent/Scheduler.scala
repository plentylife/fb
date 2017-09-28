package plenty.agent

import plenty.agent.logic.AgentManager
import plenty.executionContext
import plenty.network.Network

import scala.concurrent.Future

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
      ap.getAgentToModify() foreach { a ⇒
        var ua = a
        AgentManager.takeBids(ua)
        ua = AgentManager.applyDemurrage(ua)
        ap.set(ua)
      }
    })
  }
}

object Scheduler extends Scheduler
