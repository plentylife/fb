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
case class Coin(id: Long, belongsTo: Node, mintTime: Long, lastTransactionTime: Long) extends EquatableById[Long]

/**
  * Represents a donation made by a node
  * Currently has no privacy constraints
  **/
case class Donation(id: String, description: Array[DescriptionToken] = Array(), by: Node, timestamp: Long) extends
  HasId[String]

/**
  * Represents a chuck of a description (usually a word, or periods, or commas) that optionally can be highlighted
  **/
case class DescriptionToken(token: String, isTagged: Boolean)


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

trait Rejection[T] {
  val reason: String
  val payload: T
}

case class RejectedBid(reason: String, @deprecated bid: Bid) extends Rejection[Bid] {
  override val payload: Bid = bid
}

case class RejectedTransaction[T <: Transaction](reason: String, @deprecated transaction: T) extends Rejection[T] {
  override val payload: T = transaction
}