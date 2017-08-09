package plenty.network

import plenty.agent.model.Agent
import plenty.agent.{AgentManager, AgentPointer}
import plenty.network.communication.{CommsManager, Message}
import plenty.state.model.Node

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Failure

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
  private var incomingMessageQueue: Queue[Future[_]] = Queue.empty

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

  def clearQueue = {
    outgoingMessageQueue = outgoingMessageQueue.filterNot(_.isCompleted)
    incomingMessageQueue = incomingMessageQueue.filterNot(_.isCompleted)
  }

  def totalMessageCountInQueue = {
    println(s"messages in queue: in - ${incomingMessageQueue.size} out - ${outgoingMessageQueue.size}")
    println(s"non-completes: ${nonCompletes.mkString(" ")}")
    outgoingMessageQueue.size + incomingMessageQueue.size
  }

  var i = 0
  var nonCompletes = Set[String]()
  private def removeNonComplete(id: String) = synchronized {nonCompletes -= id}
  private def addNonComplete(id:String) = synchronized {nonCompletes += id}

  def receive(msg: Message[_]) = {
    val agentPointer = agents.find(_.id == msg.to.id).get
    val p = Promise[Agent]()
    agentPointer.getAgentToModify(p)
    //    println(s"receive $msg")
    //    println(s"\t ${msg.payload}")
    //    println(s"looking to get ${agentPointer.id}")
    val rid = Seq(i, msg.from.id, msg.to.id).mkString("-")
    addNonComplete(rid)
    i += 1
    println(s"receive $rid $msg")

    val f = p.future.map(agent => {
      //      println(s"got agent ${agent.id}")
      val agentAfterReception = CommsManager.receive(msg, toAgent = agent)
      agentPointer.set(agentAfterReception)
      //      println(s"set agent ${agent.id}")
    })
    f.onComplete({
      case Failure(e: Throwable) => {
        Network.throwErrorFromMsg(e)
      }
      case _ =>
        removeNonComplete(rid)
    })
    incomingMessageQueue.enqueue(f)
  }

  def registerAgent(agent: Agent) = {
    println(s"registering ${agent.id}")
    agents += new AgentPointer(agent)
    agentNodes += AgentManager.agentAsNode(agent)
  }

  def getAgents = agents

  private def throwErrorFromMsg(e: Throwable) = throw e
}
