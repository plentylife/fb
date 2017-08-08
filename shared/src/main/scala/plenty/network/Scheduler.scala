package plenty.network

import plenty.agent.AgentManager

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Runs CRON type tasks
  */
trait Scheduler {

  protected val cycleTime = 12 * 60 * 60 * 1000

  Future {
    while (true) {
      Thread.sleep(cycleTime)
      Future(execute())
    }
  }

  def execute() = {
    Network.getAgents.foreach(ap => {
      ap.executeWhenAvailable(a => {
        AgentManager.acceptBids(a)
      })
    })
  }
}

object Scheduler extends Scheduler
