package plenty.network

import plenty.agent.model.Agent
import plenty.agent.{AgentManager, AgentPointer}
import plenty.state.model.Node

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Failure

/**
  * Created by anton on 8/8/17.
  */
object Network {
  /* basic functions */
  def send(msg: Message[_]): Unit = {
    var rid = -1
    val msgF = Future {
      rid = addNonComplete()
//      Network.receive(msg)
      val ap = nodeToAgent(msg.to)
      agents(ap).send(msg)
    }
    msgF.onComplete {
      case Failure(e: Throwable) => {
        Network.throwErrorFromMsg(e)
      }
      case _ => removeNonComplete(rid)
    }
  }

  def receive(msg: Message[_]) = {
    val agentPointer = getAgents.find(_.id == msg.to.id).get
    val p = Promise[Agent]()
    agentPointer.getAgentToModify(p)
    //    println(s"receive $msg")
    //    println(s"\t ${msg.payload}")
    //    println(s"looking to get ${agentPointer.id}")

    var rid = -1
    val f = p.future.map(agent => {
      rid = addNonComplete()
      //      println(s"got agent ${agent.id}")
      val agentAfterReception = Receiver.receive(msg)(agent)
      agentPointer.set(agentAfterReception)
      //      println(s"set agent ${agent.id}")
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

  /* message queue tracking */

  private var i = 0
  private var _nonCompletes = Set[Int]()
  private def removeNonComplete(id: Int) = synchronized {_nonCompletes -= id}
  private def addNonComplete(): Int = synchronized {
    val id = i
    _nonCompletes += id
    i += 1
    return id
  }
  def nonCompletes = _nonCompletes

  private def throwErrorFromMsg(e: Throwable) = throw e
}
