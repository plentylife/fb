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
  var send: (Message[_]) => Unit = null

  def receive(msg: Message[_], toAgent: Agent): Agent = {
    Receiver.receive(msg)(toAgent)
  }

  def sendTransaction(t: Transaction) = {
    val msg = Message.createMessage(fromNode = t.from, toNode = t.to,
      msgPayloadId = ActionIdentifiers.SETTLE_BID_ACTION, msgPayload = t)
    send(msg)
  }

  def toSelfAndOther[T](payload: T, payloadId: PayloadIdentifier[T], self: Node, other: Node) = {
    val msgSelf = Message.createMessage(fromNode = self, toNode = self, payloadId, payload)
    val msgOther = Message.createMessage(fromNode = self, toNode = other, payloadId, payload)
    send(msgSelf)
    send(msgOther)
  }

  def toSelf[T](payload: T, payloadId: PayloadIdentifier[T], self: Node) = {
    val m = Message.createMessage(fromNode = self, toNode = self, payloadId, payload)
    send(m)
  }
}
