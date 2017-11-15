package fb

import java.io.{BufferedOutputStream, FileOutputStream, PrintWriter}
import java.util.logging.Logger

import io.circe.generic.semiauto._
import io.circe.parser.decode
import io.circe.syntax._
import plenty.agent.AgentPointer
import plenty.executionContext
import plenty.state.model.{Donation, Node}

import scala.concurrent.Future
/**
  * Created by anton on 8/15/17.
  */
object FbState {
  private val logger = Logger.getLogger("FbState")

  private val storedIn = s"./data-stores/fb_state.plenty"

  /** fb user id -> donation */
  private var donationsInProgress = Map[Node, Donation]()

  /** agent -> donation */
  private var bidsInProgress = Map[AgentPointer, Donation]()

  /** donation ids that have been given the coins for */
  private var settledDonationBonuses = Set[String]()

  /** donation -> user id, for shares that have been given the coins */
  private var settledShareBonuses = Map[String, Set[String]]()

  private var hadWebviewIntro = Set[String]()

  def getDonation(node: Node): Option[Donation] = {
    donationsInProgress.get(node)
  }

  def donationExists(node: Node) = donationsInProgress.contains(node)

  /** drops donation from tracking
    * @return optionally a tracked donation by given `node`*/
  def finishDonation(node: Node): Option[Donation] = {
    val d = donationsInProgress.get(node)
    donationsInProgress -= node
    d
  }

  /** adds or updates a donation to be tracked */
  def trackInProgress(donation: Donation) = synchronized {
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

  def settleDonationBonus(d: Donation) = synchronized {
    settledDonationBonuses += d.id
    save()
  }

  /** @param userId user that made the bid, not the referrer */
  def settleShareBonus(userId: String, donationId: String) = synchronized {
    settledShareBonuses += donationId → {settledShareBonuses.getOrElse(donationId, Set()) + userId}
    save()
  }
  def isSettled(d: Donation) = settledDonationBonuses.contains(d.id)
  def isSettled(userId: String, donationId: String): Boolean =
    settledShareBonuses.get(donationId).exists(_.contains(userId))

  def hadIntro(userId: String): Boolean = {
    val flag = hadWebviewIntro.contains(userId)
    hadWebviewIntro += userId
    save()
    flag
  }

  /* saving / loading */

  implicit val decoder = deriveDecoder[Stored]
  implicit val encoder = deriveEncoder[Stored]

  def load() = {
    val source = scala.io.Source.fromFile(storedIn).mkString
    decode[Stored](source).fold(e ⇒ {
      logger.warning(s"Could not load stored FbState. ${e.getMessage}")
    }, s ⇒ {
      settledDonationBonuses = s.settledDonationBonuses
      settledShareBonuses = s.settledShares
      hadWebviewIntro = s.hadWebviewIntro
      logger.finer("Loaded FbState from file")
    })
  }

  def save(): Unit = synchronized {
    Future {
      val buffer = new BufferedOutputStream(new FileOutputStream(storedIn))

      val pickle = Stored(settledDonationBonuses, settledShareBonuses, hadWebviewIntro).asJson.noSpaces

      val w = new PrintWriter(buffer)
      w.write(pickle)
      w.flush()
      w.close()

      buffer.close()
    }
  }

  protected[fb] case class Stored(settledDonationBonuses: Set[String], settledShares: Map[String, Set[String]],
                                  hadWebviewIntro: Set[String])

}

