package state.model

/**
  * Created by anton on 8/4/17.
  */
case class State(
                  val nodes: Set[Node] = Set[Node](),
                  val coins: Set[Coin] = Set[Coin](),
                  val donations: Set[Donation] = Set[Donation](),
                  val bids: Set[Bid] = Set(),
                  val nonSettledBids: Set[Bid] = Set()
                )

case class Log(
                val donations: Set[Donation] = Set[Donation](),
                val bids: Set[Bid] = Set(),
                val coins: Set[Coin] = Set[Coin]()
              )
