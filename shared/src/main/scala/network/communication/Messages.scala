package network.communication

import java.security.SecureRandom
import java.util.Date

import agent.model.Agent
import state.model.{Bid, Donation, Node, Transaction}

/**
  * Various messaging formats
  */
abstract class Message[P] {
  val from: Agent
  val to: Agent
  val payloadId: PayloadIdentifier[P]
  val payload: P
  val id: String
  val timestamp: Long

  /** [[state.model.Node]] that have relayed on this message */
  val relayNodes: List[Node] = List()
}

trait PayloadIdentifier[P] {
  val typeOfMsg: String
}

trait MessagePrototype[P] extends Function1[Agent, Message[P]]


object DonateAction extends PayloadIdentifier[Donation] {
  override val typeOfMsg: String = "DONATE_ACTION"
}

object BidAction extends PayloadIdentifier[Bid] {
  override val typeOfMsg: String = "BID_ACTION"
}

object BidAcceptAction extends PayloadIdentifier[Bid] {
  override val typeOfMsg: String = "BID_ACCEPT_ACTION"
}

object BidActions {
  val BidRelayDeny = Message.createAction("BID_RELAY_DENY")

}

object TransactionAction extends PayloadIdentifier[Transaction] {
  override val typeOfMsg: String = "TRANSACTION_ACTION"
}

object TransactionProbeAction extends PayloadIdentifier[Transaction] {
  override val typeOfMsg: String = "TRANSACTION_PROBE_ACTION"
}

private object Message {
  private val generator = new SecureRandom()

  def createAction[Type](constant: String) = new PayloadIdentifier[Type] {
    override val typeOfMsg: String = constant
  }

  def createMessage[Type, TypeId <: PayloadIdentifier[Type]](fromAgent: Agent, toAgent: Agent,
                                                             msgPayloadId: TypeId,
                                                             msgPayload: Type):
  Message[Type] = {
    val now = new Date().getTime
    val msgId = Seq(generator.nextInt(Int.MaxValue), now).mkString("-")

    new Message[Type] {
      override val payloadId: PayloadIdentifier[Type] = msgPayloadId
      override val payload: Type = msgPayload
      override val from: Agent = fromAgent
      override val to: Agent = toAgent
      override val id: String = msgId
      override val timestamp: Long = now
    }
  }
}
