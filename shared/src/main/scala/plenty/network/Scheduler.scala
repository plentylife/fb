package plenty.network

import plenty.agent.AgentManager
import plenty.agent.model.Agent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

/**
  * Runs CRON type tasks
  */
trait Scheduler {

  protected val cycleTime = 12 * 60 * 60 * 1000

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
      p.future.map(a => {
        AgentManager.acceptBids(a)
        // todo doesn't actually do any modification
        ap.set(a)
      })
    })
  }
}

//object Scheduler extends Scheduler
