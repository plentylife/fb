package plenty.network

import java.security.SecureRandom
import java.util.Date

import plenty.state.model._

/**
  * Various messaging formats
  */
abstract class Message[P] {
  val from: Node
  val to: Node
  val payloadId: PayloadIdentifier[P]
  val payload: P
  val id: String
  val timestamp: Long

  /** [[plenty.state.model.Node]] that have relayed on this message */
  val relayNodes: List[Node] = List()

  override def toString: String = {
    val fromStr = if (from != null) from.id else "???"
    fromStr + " -> " + to.id + "  " + payloadId.typeOfMsg + " @ " + timestamp
  }
}

trait PayloadIdentifier[P] {
  val typeOfMsg: String

  override def toString: String = typeOfMsg

  override def equals(o: scala.Any): Boolean = o match {
    case pid: PayloadIdentifier[_] => pid.typeOfMsg == this.typeOfMsg
    case _ => false
  }
}

trait MessagePrototype[P] extends Function1[Node, Message[P]]


object RelayIdentifiers {
  val DONATION_RELAY = Message.createAction[Donation]("DONATION_RELAY")
  val BID_RELAY = Message.createAction[Bid]("BID_RELAY")
}

object ActionIdentifiers {
  val REGISTER_NODE = Message.createAction[Node]("REGISTER_NODE")

  /* coin management */
  val COINS_MINTED = Message.createAction[Set[Coin]]("COINS_MINTED")

  /* bid to transfer */
  val BID_TAKE_ACTION = Message.createAction[Bid]("BID_TAKE_ACTION")
  val SETTLE_BID_ACTION = Message.createAction[Transaction]("SETTLE_BID_ACTION")
  val DENY_SETTLE_BID_ACTION = Message.createAction[RejectedTransaction]("DENY_SETTLE_BID_ACTION")
  val APPROVE_SETTLE_BID_ACTION = Message.createAction[Transaction]("APPROVE_SETTLE_BID_ACTION")

  /* bidding */
  /** a message back signifying that a bid has been accepted for consideration */
  val ACCEPT_BID_ACTION = Message.createAction[Bid]("ACCEPT_BID_ACTION")
  /** the bid has NOT been accepted for consideration for some reason such as low balance */
  val REJECT_BID_ACTION = Message.createAction[RejectedBid]("REJECT_BID_ACTION")
  /** retracting a bid */
  val RETRACT_BID_ACTION = Message.createAction[Bid]("RETRACT_BID_ACTION")
}

object DonateAction extends PayloadIdentifier[Donation] {
  override val typeOfMsg: String = "DONATE_ACTION"
}

object BidAction extends PayloadIdentifier[Bid] {
  override val typeOfMsg: String = "BID_ACTION"
}

object TransactionAction extends PayloadIdentifier[Transaction] {
  override val typeOfMsg: String = "TRANSACTION_ACTION"
}


object Message {
  private val generator = new SecureRandom()

  def createAction[Type](constant: String) = new PayloadIdentifier[Type] {
    override val typeOfMsg: String = constant
  }

  def createMessage[Type, TypeId <: PayloadIdentifier[Type]](fromNode: Node, toNode: Node,
                                                             msgPayloadId: TypeId,
                                                             msgPayload: Type):
  Message[Type] = {
    val now = new Date().getTime
    val msgId = Seq(generator.nextInt(Int.MaxValue), now).mkString("-")

    new Message[Type] {
      override val payloadId: PayloadIdentifier[Type] = msgPayloadId
      override val payload: Type = msgPayload
      override val from = fromNode
      override val to = toNode
      override val id: String = msgId
      override val timestamp: Long = now
    }
  }
}
