package plenty.state.model

import java.security.PublicKey
import java.sql.Timestamp
import java.util.Date

/**
  * Representation of a node in a network (such as a friend)
  */
case class Node(id: String)

/**
  * Representation of a coin (aka Thanks). Each coin has the same value as any other coin.
  * Coins gradually decay, and become invalid at deathTime
  * @param mintTime the Unix epoch instant
  * @param deathTime the Unix epoch instant
  * */
case class Coin(id: String, belongsTo: Node, mintTime: Long,
                deathTime: Long, wrapsAround: Option[Coin], approvedBy:Iterable[Node])

/**
  * Represents a donation made by a node
  * Currently has no privacy constraints
  * */
case class Donation(id: String, title: Option[String] = None,
                    who: Option[String] = None, what: Option[String] = None, where: Option[String] = None, when: Option[String] = None, how: Option[String] = None,
                    why: Option[String] = None,
                    attachments: Seq[String] = Seq(), by: Node, timestamp: Long)

/**
  * Represents a bid for a donation
  * @param amount can be anything >= 0
  * */
case class Bid(id: String, donation: Donation, amount: Int, by: Node, timestamp: Long)

/**
  * Represents a transaction of [[plenty.state.model.Coin]] between two [[plenty.state.model.Node]]s
  * */
case class Transaction(id: String, timestamp: Long, coins: Set[Coin], from: Node, to: Node, bid: Option[Bid] = None)

// Supplementary

trait Rejection {
  val reason: String
}

case class RejectedBid(reason: String, bid: Bid) extends Rejection

case class RejectedTransaction(reason: String, transaction: Transaction) extends Rejection