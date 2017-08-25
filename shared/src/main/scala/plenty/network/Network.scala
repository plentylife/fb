package plenty.network

import plenty.agent.model.Agent
import plenty.agent.{AgentManager, AgentPointer}
import plenty.state.model.Node

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Failure

/**
  * Created by anton on 8/8/17.
  */
object Network {

  /* basic functions */

  def send(msg: Message[_]): Unit = {
    val rid = addNonComplete(msg)
//    println(s"send rid $rid | $msg")

    val msgF = Future {
      //      Network.receive(msg)
      val ap = nodeToAgent(msg.to)
      agents(ap).send(msg)
    }
    msgF.onComplete {
      case Failure(e: Throwable) =>
        removeNonComplete(rid)
        Network.throwErrorFromMsg(e)
      case _ =>
        removeNonComplete(rid)
    }
  }

  def receive(msg: Message[_]) = {
    val rid = addNonComplete(msg)
//    println(s"receive rid $rid | $msg")

    val agentPointer = getAgents.find(_.id == msg.to.id).get
    val p = Promise[Agent]()
    agentPointer.getAgentToModify(p)
    //        println(s"\t ${msg.payload}")
    //    println(s"looking to get ${agentPointer.id}")

    val f = p.future.map(agent => {
      val agentAfterReception = Receiver.receive(msg)(agent)
      agentPointer.set(agentAfterReception)
    })
    f.onComplete({
      case Failure(e: Throwable) => {
        removeNonComplete(rid)
        Network.throwErrorFromMsg(e)
      }
      case _ =>
        removeNonComplete(rid)
    })
  }

  /* utility functions */

  def notifyAllAgents[P](payload: P, payloadId: PayloadIdentifier[P], from: Node) = {
    val unaddressedMsg = (to: Node) => Message.createMessage(fromNode = from, toNode = to, msgPayloadId = payloadId,
      msgPayload = payload)
    for (ap <- getAgents) {
      val msg = unaddressedMsg(AgentManager.agentAsNode(ap.getAgentInLastKnownState))
      send(msg)
    }
  }

  /* agent registration */

  private var agents: Map[AgentPointer, SendInterface] = Map()
  private var nodeToAgent: Map[Node, AgentPointer] = Map()

  def registerAgent(agent: Agent, interface: SendInterface): AgentPointer = {
    println(s"registering ${agent.id}")
    val pointer = new AgentPointer(agent)
    agents += pointer -> interface
    nodeToAgent += pointer.node -> pointer
    pointer
  }

  def getAgents: Set[AgentPointer] = agents.keySet

  def clear = {
    agents = Map()
    nodeToAgent = Map()
  }

  /* message queue tracking */

  private var msgCounter: Long = 0
  private val resetCounterAt = Long.MaxValue - 2
  private var _nonCompletes = Map[Long, Message[_]]()

  /** after returning, increments value by 1 and makes sure that it is not beyond bounds of Long
    * @return the current value of the counter */
  private def touchCounter: Long = synchronized {
    val current = msgCounter
    if (current >= resetCounterAt) {
      msgCounter = 0
    } else {
      msgCounter += 1
    }
    return current
  }

  private def removeNonComplete(id: Long) = this.synchronized {
//    println(s"trying to remove $id")
    _nonCompletes -= id
  }

  private def addNonComplete(msg: Message[_]): Long = this.synchronized {
    val id = touchCounter
    _nonCompletes += id → msg
    id
  }

  def nonCompletes: Map[Long, Message[_]] = _nonCompletes

  private def throwErrorFromMsg(e: Throwable) = throw e
}
