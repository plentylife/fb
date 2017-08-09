package plenty.network

import plenty.agent.{AgentManager, AgentPointer}
import plenty.agent.model.Agent
import plenty.network.communication.{CommsManager, Message}
import plenty.state.model.Node

import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Try}

/**
  * Created by anton on 8/8/17.
  */
object Network {
  // registering sender method
  CommsManager.sender = send
  CommsManager.getAllAgentsInNetwork = () => agentNodes


  private var agents: Set[AgentPointer] = Set()
  private var agentNodes: Set[Node] = Set()
  private var outgoingMessageQueue: Queue[Future[_]] = Queue.empty

  def send(msg: Message[_]): Unit = {
    val msgF = Future(Network.receive(msg))
    msgF.onComplete {
      case Failure(e: Throwable) => {
        Network.throwErrorFromMsg(e)
      }
      case _ => null
    }
    outgoingMessageQueue = outgoingMessageQueue.enqueue(msgF)
    clearQueue
  }

  def clearQueue = outgoingMessageQueue = outgoingMessageQueue.filterNot(_.isCompleted)

  def messageCountInQueue = outgoingMessageQueue.size

  def receive(msg: Message[_]) = {
    val agentPointer = agents.find(_.id == msg.to.id).get
    val p = Promise[Agent]()
    agentPointer.getAgentToModify(p)
    p.future.map(agent => {
      val agentAfterReception = CommsManager.receive(msg, toAgent = agent)
      agentPointer.set(agentAfterReception)
    })
  }

  def registerAgent(agent: Agent) = {
    println("reg a", agent.id)
    agents += new AgentPointer(agent)
    agentNodes += AgentManager.agentAsNode(agent)
  }

  def getAgents = agents

  private def throwErrorFromMsg(e: Throwable) = throw e
}
