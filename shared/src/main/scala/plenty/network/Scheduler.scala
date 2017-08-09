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

  Future {
    while (true) {
      Thread.sleep(cycleTime)
      Future(execute())
    }
  }

  def execute() = {
    Network.getAgents.foreach(ap => {
      val p = Promise[Agent]()
      ap.getAgentToModify(p)
      p.future.map(a => {
        AgentManager.acceptBids(a)
      })
    })
  }
}

//object Scheduler extends Scheduler
