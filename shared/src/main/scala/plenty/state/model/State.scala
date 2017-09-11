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
                  val chains: Chains = Chains()
//                  val relay: Relay = Relay()
                )

 case class Chains(
                val donations: List[Donation] = List[Donation](),
                val bids: List[Bid] = List(),
                 /** a way to quickly track where each coin is */
//                val transactionsByCoin: Map[Coin, List[Transaction]] = Map(),
                val transactions: List[Transaction] = List()
              )

 case class Relay(
                  val donations: Set[Donation] = Set[Donation]()
                )
