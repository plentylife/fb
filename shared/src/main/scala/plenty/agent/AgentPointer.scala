package plenty.agent

import plenty.agent.model.Agent

import scala.collection.immutable.Queue
import scala.concurrent.{Future, Promise}
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Allows safe parallel modification and retrieval of [[plenty.agent.model.Agent]]s
  */
class AgentPointer(private var agent: Agent) {
  private var promiseOfAgent: Promise[Agent] = Promise()

  private var queue: Queue[(Agent) => Any] = Queue.empty

  private var hasExecutedOnce = false

  def id = agent.id

  def executeWhenAvailable(execute: (Agent) => Any) = {
    queue = queue.enqueue(execute)
    if (!hasExecutedOnce) {
      hasExecutedOnce = true
      set(agent)
    }
  }

  def getLast = promiseOfAgent.future

  private def refreshPromise() = {
    promiseOfAgent = Promise()
    promiseOfAgent.future.onComplete({
      case Success(agent) =>
        val (func, q) = queue.dequeue
        queue = q
        func(agent)
    })
  }

  def set(a: Agent) = synchronized {
    promiseOfAgent success a
    refreshPromise()
  }

}
