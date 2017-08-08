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

  var getAllAgentsInNetwork: () => Iterable[Node] = null

  def receive(msg: Message[_], toAgent: Agent): Agent = {
    Receiver.receive(msg)(toAgent)
  }

  def beacon(msgProto: MessagePrototype[_]) = {
    getAllAgentsInNetwork().foreach(n => {
      sender(msgProto(n))
    })
  }

  def relay(msgProto: MessagePrototype[_], from: Node) = {
    getAllAgentsInNetwork().filterNot(_.id == from.id).foreach(n => {
      sender(msgProto(n))
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

  def toSelfAndOther[T](payload: T, payloadId: PayloadIdentifier[T], self: Node, other: Node) = {
    val msgSelf = Message.createMessage(fromNode = self, toNode = self, payloadId, payload)
    val msgOther = Message.createMessage(fromNode = self, toNode = other, payloadId, payload)
    sender(msgSelf)
    sender(msgOther)
  }

  def basicRelay[T](payload: T, payloadId: PayloadIdentifier[T], from: Node) = {
    relay(new MessagePrototype[T] {
      override def apply(v1: Node): Message[T] = Message.createMessage(fromNode = from, toNode = v1, payloadId, payload)
    }, from)
  }
}
