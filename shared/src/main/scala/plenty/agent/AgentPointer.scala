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
  private var queue: Queue[Promise[Agent]] = Queue.empty

  private var agentAvailable = true

  def id = agent.id

  def getAgentToModify(promise: Promise[Agent]) = {
    queue = queue.enqueue(promise)
    giveAgent()
  }

  def getLastAgent = agent

  def set(a: Agent) = synchronized {
    agentAvailable = true
    agent = a
    giveAgent()
  }

  private def giveAgent() = if (agentAvailable) {
    agentAvailable = false
    val (p, q) = queue.dequeue
    p success agent
  }

}
