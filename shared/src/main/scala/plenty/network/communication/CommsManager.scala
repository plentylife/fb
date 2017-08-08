package plenty.network.communication

import java.util.Date

import plenty.agent.AgentManager
import plenty.agent.model.Agent
import plenty.state.model.{Bid, Node, Transaction}

import scala.concurrent.Future

/**
  * All communication requests are channeled through this module
  */
object CommsManager {
  var sender: (Message[_]) => Unit = null

  var getAllAgentsInNetwork: () => Iterable[Agent] = null

  def receive(msg: Message[_], toAgent: Agent): Agent = {
    Receiver.receive(msg)(toAgent)
  }

  def beacon(msgProto: MessagePrototype[_]) = {
    getAllAgentsInNetwork().foreach(a => {
      sender(msgProto(AgentManager.agentAsNode(a)))
    })
  }

  def relay(msgProto: MessagePrototype[_], from: Node) = {
    getAllAgentsInNetwork().filterNot(_.id == from.id).foreach(a => {
      sender(msgProto(AgentManager.agentAsNode(a)))
    })
  }

  def transact(transaction: Transaction, fromAgent: Agent) = {
    val msgProto = new MessagePrototype[Transaction] {
      override def apply(to: Node): Message[Transaction] = {
        Message.createMessage(AgentManager.agentAsNode(fromAgent), to, TransactionAction, transaction)
      }
    }

    beacon(msgProto)
  }

  def acceptBidBeacon(bid: Bid, fromAgent: Agent) = {
    val msgProto = new MessagePrototype[Bid] {
      override def apply(to: Node): Message[Bid] = {
        Message.createMessage(fromNode = AgentManager.agentAsNode(fromAgent), to, BidAcceptAction, bid)
      }
    }

    beacon(msgProto: MessagePrototype[Bid])
  }

  def basicRelay[T](payload: T, payloadId: PayloadIdentifier[T], from: Node) = {
    relay(new MessagePrototype[T] {
      override def apply(v1: Node): Message[T] = Message.createMessage(fromNode = from, toNode = v1, payloadId, payload)
    }, from)
  }
}
