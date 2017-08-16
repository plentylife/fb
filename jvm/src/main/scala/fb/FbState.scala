package fb

import plenty.state.StateManager
import plenty.state.model.{Donation, Node}

/**
  * Created by anton on 8/15/17.
  */
object FbState {

  /** fb user id -> donation */
  private var donationsInProgress = Map[Node, Donation]()

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

}
