package plenty.agent.logic

import java.util.logging.Level

import com.softwaremill.quicklens._
import plenty.agent.model._
import plenty.state.StateManager
import plenty.state.model._

/**
  * Facilitates interaction between two [[plenty.agent.model.Agent]]
  */
object StateLogic {
  private val logger = java.util.logging.Logger.getLogger("StateLogic")
  logger.setLevel(Level.ALL)

  def registerNode(node: Node, agent: Agent): Agent = {
    if (node != agent.node) {
      var s = agent.state
      s = s.copy(nodes = s.nodes + node)
      val a = agent.copy(state = s)
      StateManager.save(a)
      a
    } else agent
  }

  def registerCoins(coins: Set[Coin], agent: Agent): Agent = {
    var s = agent.state
    // removing old coins states
    var stateCoins = s.coins diff coins
    // replacing with new coin states
    stateCoins ++= coins

    s = s.copy(coins = stateCoins)
    val a = agent.copy(state = s)
    StateManager.save(a)
    a
  }

  def registerTakenBid(bid: Bid, agent: Agent): Agent = {
    agent.state.bids find (_ == bid) map { b ⇒
      val stateUpdated = agent.state.copy(
        // it is important to remove that bid from bids, so that it cannot be transacted again
        bids = agent.state.bids - b,
        bidsPendingSettle = agent.state.bidsPendingSettle + b
      )
      val agentUpdated = agent.copy(state = stateUpdated)

      StateManager.save(agentUpdated)
      agentUpdated
    } getOrElse {
      logger.info(s"Bid not found. $bid")
      agent
    }
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
    val nonSettledBids = s.bidsPendingSettle filterNot (_ == bid)
    s = s.copy(bidsPendingSettle = nonSettledBids, bids = bids)
    agent.copy(state = s)
  }

  def registerApprovedBidSettle(t: BidTransaction, agent: Agent): Agent = {
    var s = agent.state
    val bid = t.bid
    val bids = s.bids filterNot {_.donation == bid.donation}
    val nonSettledBids = s.bidsPendingSettle filterNot (_ == bid)
    val donations = s.donations filterNot(_ == bid.donation)

    s = s.copy(bids = bids, bidsPendingSettle = nonSettledBids, donations = donations)
    agent.copy(state = s)
  }

  /**
    * Pushes the transaction into the chain for permanent storage
    * Clears the transaction from pending
    * The transaction must be verified by this point
    */
  def onTransactionFinish(t: Transaction, agent: Agent): Agent = {
    val a = agent.modify(_.state.chains.transactions).using(list ⇒ t +: list)
      .modify(_.state.transactionsPendingSettle).using(pts ⇒ pts - t)
    StateManager.save(a)
    a
  }

  /** Used when a transaction is sent out; keeps track of it as pending. */
  def onTransact(t: Transaction, a: Agent): Agent = {
    val ua = a.modify(_.state.transactionsPendingSettle).using(pts ⇒ pts + t)
    StateManager.save(ua)
    a
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
