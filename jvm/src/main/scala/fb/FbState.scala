package fb

import plenty.agent.AgentPointer
import plenty.state.StateManager
import plenty.state.model.{Bid, Donation, Node}

/**
  * Created by anton on 8/15/17.
  */
object FbState {
  /** fb user id -> donation */
  private var donationsInProgress = Map[Node, Donation]()

  /** agent -> donation */
  private var bidsInProgress = Map[AgentPointer, Donation]()

  def getOrCreateDonation(node: Node): Donation = {
    donationsInProgress.get(node) match {
      case Some(d) => d
      case _ =>
        val d = StateManager.createDonation("", "", Nil, node)
        update(d)
        d
    }
  }

  def donationExists(node: Node) = donationsInProgress.contains(node)
  def finishDonation(node: Node): Option[Donation] = {
    val d = donationsInProgress.get(node)
    donationsInProgress -= node
    d
  }
  def update(donation: Donation) = synchronized {
    donationsInProgress += (donation.by -> donation)
  }

  def trackBid(a: AgentPointer, d: Donation) = synchronized {
    bidsInProgress += a -> d
  }

  def popBid(a: AgentPointer): Option[Donation] = synchronized {
    val d = bidsInProgress.get(a)
    if (d.nonEmpty) bidsInProgress -= a
    d
  }
}
