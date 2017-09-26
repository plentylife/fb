package plenty.state.model

/**
  * Representation of a node in a network (such as a friend)
  */
case class Node(id: String) extends HasId[String]

/**
  * Representation of a coin (aka Thanks). Each coin has the same value as any other coin.
  * Coins gradually decay, and become invalid at deathTime
  *
  * @param mintTime            the Unix epoch instant
  * @param lastTransactionTime Unix epoch instant of the last transaction
  **/
case class Coin(id: Long, belongsTo: Node, mintTime: Long, lastTransactionTime: Long) extends EquatableById[Long] {
//  override def equals(o: Any): Boolean = super.equals(o)
}

/**
  * Represents a donation made by a node
  * Currently has no privacy constraints
  **/
case class Donation(id: String, title: Option[String] = None,
                    who: Option[String] = None, what: Option[String] = None, where: Option[String] = None,
                    when: Option[String] = None, how: Option[String] = None, why: Option[String] = None,
                    attachments: Seq[String] = Seq(), by: Node, timestamp: Long) extends HasId[String]

/**
  * Represents a bid for a donation
  *
  * @param amount can be anything >= 0
  **/
case class Bid(id: String, donation: Donation, amount: Int, by: Node, timestamp: Long) extends HasId[String]


trait Transaction extends HasId[String] {
  val transactionType: TransactionType.Value

  val id: String
  val timestamp: Long
  val coins: Set[Coin]
  val from: Node
  val to: Node
  val signatureFrom: String = ""
  val signatureTo: String = ""
}

/**
  * Represents a transaction of [[plenty.state.model.Coin]] between two [[plenty.state.model.Node]]s
  **/
case class BaseTransaction(id: String, timestamp: Long, coins: Set[Coin], from: Node, to: Node) extends
  Transaction {val transactionType = TransactionType.BASE}

case class DemurageTransaction(id: String, timestamp: Long, coins: Set[Coin], from: Node, to: Node) extends
  Transaction {val transactionType = TransactionType.DEMURAGE}

/** Represent a transaction that was an outcome of a bid */
case class BidTransaction(id: String, timestamp: Long, coins: Set[Coin], from: Node, to: Node, bid:
Bid) extends Transaction {override val transactionType = TransactionType.BID}

object TransactionType extends Enumeration {
  val BID, BASE, DEMURAGE = Value
}

// Supplementary

trait Rejection {
  val reason: String
}

case class RejectedBid(reason: String, bid: Bid) extends Rejection

case class RejectedTransaction[T <: Transaction](reason: String, transaction: T) extends Rejection