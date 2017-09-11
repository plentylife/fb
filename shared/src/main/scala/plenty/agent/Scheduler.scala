package plenty.agent

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
      val agent = ap.agentInLastState
      AgentManager.takeBids(agent)
      ActionLogic.applyDemurage(agent)
    })
  }
}

object Scheduler extends Scheduler
