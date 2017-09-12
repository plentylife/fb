package plenty.agent

import plenty.agent.model._
import plenty.network._
import plenty.state.StateManager
import plenty.state.model._
import com.softwaremill.quicklens._

/**
  * Facilitates interaction between two [[plenty.agent.model.Agent]]
  */
object StateLogic {

  def registerNode(node: Node, agent: Agent): Agent = {
    var s = agent.state
    s = s.copy(nodes = s.nodes + node)
    val a = agent.copy(state = s)
    StateManager.save(a)
    a
  }

  def registerCoins(coins: Set[Coin], agent: Agent): Agent = {
    var s = agent.state
    var stateCoins = s.coins ++ coins
    // removing old coins states
    stateCoins = stateCoins -- coins
    // replacing with new coin states
    stateCoins ++= coins

    s = s.copy(coins = stateCoins)
    val a = agent.copy(state = s)
    StateManager.save(a)
    a
  }

  def registerTakenBid(bid: Bid, agent: Agent): Agent = {
    val stateUpdated = agent.state.copy(
      bids = agent.state.bids.filterNot(_.id == bid.id),
      nonSettledBids = agent.state.nonSettledBids + bid
    )
    val agentUpdated = agent.copy(state = stateUpdated)

    StateManager.save(agentUpdated)
    agentUpdated
  }

  def registerBid(bid: Bid)(implicit agent: Agent): Agent = {
    val stateUpdated = agent.state.copy(
      bids = agent.state.bids + bid
    )
    val agentUpd = agent.copy(state = stateUpdated)

    StateManager.save(agentUpd)

    agentUpd
  }

  def removeBid(bid: Bid, agent: Agent): Agent = {
    var s = agent.state
    val bids = s.bids filterNot(_ == bid)
    val nonSettledBids = s.nonSettledBids filterNot(_ == bid)
    s = s.copy(nonSettledBids = nonSettledBids, bids = bids)
    agent.copy(state = s)
  }

  def registerApprovedBidSettle(t: BidTransaction, agent: Agent): Agent = {
    var s = agent.state
    val bid = t.bid
    val bids = s.bids filterNot {_.donation == bid.donation}
    val nonSettledBids = s.nonSettledBids filterNot(_ == bid)
    val donations = s.donations filterNot(_ == bid.donation)

    s = s.copy(bids=bids, nonSettledBids=nonSettledBids, donations=donations)
    agent.copy(state = s)
  }

  /** Changes the ownership of the coins
    * The transaction must be verified by this point */
  def finishTransaction(t: Transaction, agent: Agent): Agent = {
    val coins = Accounting.transferCoins(t)
    registerCoins(coins, agent).modify(_.state.chains.transactions).using(list ⇒ t +: list)
  }

  def donationRegistration(donation: Donation)(implicit agent: Agent): Agent = {
    val stateUpdated = agent.state.copy(
      donations = agent.state.donations + donation
    )
    val agentUpd = agent.copy(state = stateUpdated)

    StateManager.save(agentUpd)

    agentUpd
  }
}
