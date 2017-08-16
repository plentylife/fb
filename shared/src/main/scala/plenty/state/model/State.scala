package plenty.state.model

import plenty.network.Message

/**
  * Created by anton on 8/4/17.
  */
case class State(
                  val nodes: Set[Node] = Set[Node](),
                  // must remain a set!
                  val coins: Set[Coin] = Set[Coin](),
                  val donations: Set[Donation] = Set[Donation](),
                  val bids: Set[Bid] = Set(),
                  val nonSettledBids: Set[Bid] = Set(),
                  val history: History = History(),
                  val relay: Relay = Relay()
                )

case class History(
                val donations: Set[Donation] = Set[Donation](),
                val bids: Set[Bid] = Set(),
                val coins: Set[Coin] = Set[Coin]()
              )

case class Relay(
                  val donations: Set[Donation] = Set[Donation]()
                )
