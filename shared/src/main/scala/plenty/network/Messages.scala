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
    fromStr.formatted("%18s") + " -> " + to.id.formatted("%18s") + " " + payloadId.typeOfMsg.formatted("%25s") + " @ " +
      "\n\t" + payloadId.displayPayload(payload)
  }
}

trait PayloadIdentifier[P] {
  val typeOfMsg: String

  private var loggingFunction: (P) ⇒ String = { _ ⇒ "" }

  def cast(o: Any): P = o.asInstanceOf[P]

  def run[R](f: (Message[P]) ⇒ R)(implicit m: Message[_]): R = f(m.asInstanceOf[Message[P]])

  override def toString: String = typeOfMsg

  def displayPayload(payload: P): String = payload match {
    case p: HasId[_] ⇒ p.id.toString + loggingFunction(payload)
    case p ⇒ loggingFunction(p)
  }

  def setLoggingFunction(f: (P) ⇒ String): Unit = loggingFunction = f
  
  override def equals(o: scala.Any): Boolean = o match {
    case pid: PayloadIdentifier[_] => pid.typeOfMsg == this.typeOfMsg
    case _ => false
  }
}

trait MessagePrototype[P] extends Function1[Node, Message[P]]


object RelayIdentifiers {
  final val DONATION_RELAY = Message.createAction[Donation]("DONATION_RELAY")
  final val BID_RELAY = Message.createAction[Bid]("BID_RELAY")
}

object ActionIdentifiers {
  final val REGISTER_NODE = Message.createAction[Node]("REGISTER_NODE")

  /* coin management */
  final val COINS_MINTED = Message.createAction[Set[Coin]]("COINS_MINTED")

  /* transaction */
  final val TRANSACTION = Message.createAction[Transaction]("TRANSACTION")
  final val ACCEPT_TRANSACTION = Message.createAction[Transaction]("ACCEPT_TRANSACTION")
  final val REJECT_TRANSACTION = Message.createAction[RejectedTransaction[Transaction]]("REJECT_TRANSACTION")

  /* bid to transfer */
  final val BID_TAKE_ACTION = Message.createAction[Bid]("BID_TAKE_ACTION")
  final val SETTLE_BID_ACTION = Message.createAction[BidTransaction]("SETTLE_BID_ACTION")
  final val DENY_SETTLE_BID_ACTION = Message.createAction[RejectedTransaction[BidTransaction]]("DENY_SETTLE_BID_ACTION")
  final val APPROVE_SETTLE_BID_ACTION = Message.createAction[BidTransaction]("APPROVE_SETTLE_BID_ACTION")

  /* bidding */
  /** a message back signifying that a bid has been accepted for consideration */
  final val ACCEPT_BID_ACTION = Message.createAction[Bid]("ACCEPT_BID_ACTION")
  /** the bid has NOT been accepted for consideration for some reason such as low balance */
  final val REJECT_BID_ACTION = Message.createAction[RejectedBid]("REJECT_BID_ACTION")
  /** retracting a bid */
  final val RETRACT_BID_ACTION = Message.createAction[Bid]("RETRACT_BID_ACTION")
}

object DonateAction extends PayloadIdentifier[Donation] {
  override val typeOfMsg: String = "DONATE_ACTION"
}

object BidAction extends PayloadIdentifier[Bid] {
  override val typeOfMsg: String = "BID_ACTION"
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
      override val from: Node = fromNode
      override val to: Node = toNode
      override val id: String = msgId
      override val timestamp: Long = now
    }
  }
}
