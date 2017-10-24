package plenty.state

import java.security.{MessageDigest, SecureRandom}
import java.util.{Base64, Date}

import plenty.state.model._

/**
  * The entry point into state management.
  *
  */
object StateManager {

  private val random = new SecureRandom()
  private val hasher = MessageDigest.getInstance("SHA-512")

  /* Creating objects */

  private def idGenerator(time: Long, additionalPiece: String = ""): String = {
    val idStr = Seq(random.nextInt(Int.MaxValue), time).mkString("") + additionalPiece
    val idBytes = idStr.toCharArray map {_.toByte}
    val hash = hasher.digest(idBytes)
    Base64.getUrlEncoder.encodeToString(hash).replaceAll("=", "")
  }

  /** @return donation with the id, by, and timestamp filled only */
  def createEmptyDonation(by: Node): Donation = {
    val now = new Date().getTime
    val id = idGenerator(now, by.id)
    Donation(id = id, by = by, timestamp = now)
  }

  def createDonation(descr: Iterable[DescriptionToken], by: Node): Donation = {
    createEmptyDonation(by).copy(description = descr)
  }

  def createBid(donation: Donation, amount: Int, by: Node): Bid = {
    val now = new Date().getTime
    val id = idGenerator(now, amount + by.id)
    Bid(id = id, donation = donation, amount = amount, by = by, timestamp = now)
  }

  def createTransaction(coins: Set[Coin], from: Node, to: Node): Transaction = {
    val now = new Date().getTime
    val id = idGenerator(now, s"$from$to$now")
    BaseTransaction(id, now, coins, from, to)
  }

  def asDemurage(t: Transaction): DemurageTransaction = {
    DemurageTransaction(t.id, t.timestamp, t.coins, from = t.from, to = t.to)
  }

  def transformTransaction(t: Transaction, bid: Bid): BidTransaction = {
    BidTransaction(t.id, t.timestamp, t.coins, from = t.from, to = t.to, bid)
  }


  /* Selecting objects */

  /** @return bids that have the donation (of the given bid) in common.
    *         takes into account bids and non-settled bids */
  def getRelatedBids(state: State, bid: Bid): Set[Bid] = (state.bids ++ state.bidsPendingSettle) filter {
    _.donation == bid.donation
  }

  def getRelatedBids(state: State, donationId: String): Set[Bid] = (state.bids ++ state.bidsPendingSettle) filter {
    _.donation.id == donationId
  }

  def getDonation(id: String)(implicit state: State) = state.donations.find(_.id == id)

  def updateChains(state: State, newChains: Chains): State = state.copy(chains = newChains)

}

