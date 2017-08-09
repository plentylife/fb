package plenty.network

import plenty.agent.model.Agent
import plenty.agent.{AgentManager, AgentPointer}
import plenty.network.communication.{CommsManager, Message, PayloadIdentifier}
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
  CommsManager.send = send

  /* basic functions */

  def send(msg: Message[_]): Unit = {
    var rid = -1
    val msgF = Future {
      rid = addNonComplete()
      Network.receive(msg)
    }
    msgF.onComplete {
      case Failure(e: Throwable) => {
        Network.throwErrorFromMsg(e)
      }
      case _ => removeNonComplete(rid)
    }
  }

  def receive(msg: Message[_]) = {
    val agentPointer = agents.find(_.id == msg.to.id).get
    val p = Promise[Agent]()
    agentPointer.getAgentToModify(p)
    //    println(s"receive $msg")
    //    println(s"\t ${msg.payload}")
    //    println(s"looking to get ${agentPointer.id}")

    var rid = -1
    val f = p.future.map(agent => {
      rid = addNonComplete()
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
  }

  /* utility functions */
  def notifyAll[P](payload: P, payloadId: PayloadIdentifier[P], from: Node) = {
    val unaddressedMsg = (to: Node) => Message.createMessage(fromNode = from, toNode = to, msgPayloadId = payloadId,
      msgPayload =
      payload)
    for (ap <- agents) {
      val msg = unaddressedMsg(AgentManager.agentAsNode(ap.getAgentInLastKnownState))
      send(msg)
    }
  }

  /* agent registration */

  private var agents: Set[AgentPointer] = Set()
  private var agentNodes: Set[Node] = Set()
  def registerAgent(agent: Agent) = {
    println(s"registering ${agent.id}")
    agents += new AgentPointer(agent)
    agentNodes += AgentManager.agentAsNode(agent)
  }
  def getAgents = agents

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
