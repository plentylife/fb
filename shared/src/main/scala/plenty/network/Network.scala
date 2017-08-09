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

  var i = 0
  var nonCompletes = Set[Int]()
  private def removeNonComplete(id: Int) = synchronized {nonCompletes -= id}
  private def addNonComplete(): Int = synchronized {
    val id = i
    nonCompletes += id
    i += 1
    return id
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

  def registerAgent(agent: Agent) = {
    println(s"registering ${agent.id}")
    agents += new AgentPointer(agent)
    agentNodes += AgentManager.agentAsNode(agent)
  }

  def getAgents = agents

  private def throwErrorFromMsg(e: Throwable) = throw e
}
