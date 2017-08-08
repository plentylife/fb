package plenty.network

import plenty.agent.model.Agent
import plenty.network.communication.{CommsManager, Message}

import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Try}

/**
  * Created by anton on 8/8/17.
  */
object Network {
  // registering sender method
  CommsManager.sender = send
  CommsManager.getAllAgentsInNetwork = () => agents

  private var outgoingMessageQueue: Queue[Future[_]] = Queue.empty

//  private var incomingMessageQueue: Queue[Future[_]] = Queue.empty

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

  def receive(msg: Message[_]) = synchronized {
    val agent = CommsManager.receive(msg, toAgent = popAgent(msg.to.id))
    println("receiving message", agent.id, msg)
    reRegisterAgent(agent)
    println("re-registered agent", agent)
  }

  private var agents: Set[Agent] = Set()

  def registerAgent(agent: Agent) = {
    agents += agent
  }
  def popAgent(id: String): Agent = {
    val a = agents.find(_.id == id).get
    agents -= a
    a
  }
  def reRegisterAgent(agent: Agent) = {
    agents += agent
  }
  /** for testing purposes fixme */
  def getAgents = agents

  private def throwErrorFromMsg(e: Throwable) = throw e
}
