package plenty.agent

import plenty.agent.model.Agent
import plenty.network.PayloadIdentifier

import scala.collection.immutable.Queue
import scala.concurrent.Promise

/**
  * Allows safe parallel modification and retrieval of [[plenty.agent.model.Agent]]s
  */
class AgentPointer(private var agent: Agent) {
  private var queue: Queue[Promise[Agent]] = Queue.empty

  private var agentAvailable = true

  def id = agent.id
  val node = AgentManager.agentAsNode(agent)

  def getAgentToModify(promise: Promise[Agent]) = synchronized {
    queue = queue.enqueue(promise)
    giveAgent()
  }

  def getAgentInLastKnownState = agent

  def set(a: Agent) = synchronized {
    agentAvailable = true
    agent = a
//    println(s"setting agent ${agent.id}")
    giveAgent()
  }

  private def giveAgent() =
    if (agentAvailable && queue.nonEmpty) {
      agentAvailable = false
      val (p, q) = queue.dequeue
      queue = q
//      println(s"giving agent ${agent}")
      p success agent
    }

  override def equals(o: scala.Any): Boolean = o match {
    case p: AgentPointer => p.agent == this.agent
    case _ => false
  }
}
