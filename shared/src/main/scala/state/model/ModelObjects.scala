package state.model

import java.security.PublicKey
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
case class Donation(id: String, title: String, description: String, by: Node)

/**
  * Represents a bid for a donation
  * @param amount can be anything >= 0
  * */
case class Bid(id: String, donation: Donation, amount: Int, by: Node, timestamp: Long)