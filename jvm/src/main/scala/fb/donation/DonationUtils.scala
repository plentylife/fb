package fb.donation

import java.util
import java.util.concurrent.TimeUnit

import com.restfb.Parameter
import com.restfb.types.{Photo, Post}
import fb._
import plenty.agent.AgentPointer
import plenty.network.{DonateAction, Network}
import plenty.state.DonationStateUtils._
import plenty.state.StateManager
import plenty.state.model.Donation

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

private[donation] object DonationUtils {

  def startDonation(a: AgentPointer): Donation = {
    val d = StateManager.createEmptyDonation(a.node)
    FbState.trackInProgress(d)
    d
  }

  def fillOutDonationField(d: Donation, field: String, msg: DonationMessage): Option[Donation] = {
    if (field == "pictures") {
      addPictures(d, msg)
    } else fillOutTextField(d, field, msg)
  }

  /** fills out a field in a donation from the message
    * if the field is empty and not required, then fills it with a `-`
    *
    * @return copy of the donation, or [[None]] if the field is requeired, but not present */
  private def fillOutTextField(d: Donation, f: String, msg: DonationMessage): Option[Donation] = {
    if (msg.isTextEmpty && !DonationFlow.requiredFields.contains(f)) return Option(updateField(d, f, value = Some("")))
    msg.text flatMap { rawText ⇒
      val text = rawText.trim
      if (text.nonEmpty) return Option(updateField(d, f, value = Option(text)))
      None
    }
  }

  /** adds a pictures url from the message if such url exists
    *
    * @return None if there is no image url in the message, otherwise copy of the donation */
  private def addPictures(d: Donation, msg: DonationMessage): Option[Donation] = {
    val attachments = msg.attachments
    val pictures = attachments filter (_.getType == "image") map (_.getPayload.getUrl)

    if (pictures.nonEmpty)
      Option(d.copy(attachments = d.attachments ++ pictures))
    else None
  }

  /**
    * Finishes the donation, drops it from [[FbState]] in-progress tacking, and publishes it as a post on fb
    *
    * @return donation with post id
    **/
  def publishDonation(donation: Donation, a: AgentPointer): Option[(Donation, String)] = {
        val attachments = new util.ArrayList[String]()
        donation.attachments map { url =>
          val id = fbClient.publish(s"${FbSettings.pageId}/photos",
            classOf[Photo], Parameter.`with`("url", url), Parameter.`with`("published", false)
          ).getId
          s"{'media_fbid':'$id'}"
        } foreach {
          attachments.add
        }
        try {
          val bidLink = s"m.me/${FbSettings.pageId}?ref=BID_${donation.id}"
          println("about to get a short link")
          val shortBidLink = Utility.getShortLink(bidLink)
          println("got short link")
          val msg = producePostBody(donation, shortBidLink)
          val publishMessageResponse = fbClient.publish(s"${FbSettings.pageId}/feed",
            classOf[Post],
            Parameter.`with`("message", msg),
            Parameter.`with`("attached_media", attachments)
          )
          println("posted donation")
          Some(donation -> publishMessageResponse.getId)
        } catch {
          case e: Throwable =>
            Responses.errorPersonal(a, s"publishingPostInUtils $e")
            None
        }
    }

  /** @return a string with the questions and answers -- the bulk of the donation description in a post */
  private def producePostBody(d: Donation, bidUrl: String): String = {
    val title = s"${d.title.getOrElse("title is missing")}\n-----\n"
    val qAndA = DonationFlow.fieldsInPostOrder map { f ⇒ publishTextField(d, f) } mkString "\n\n"
    val bidBlock = s"\n===\n This is an open auction. \nTo enter your bid follow $bidUrl\nThis link opens messenger " +
      s"and allows you to talk to Plenty bot"
    title + qAndA + bidBlock
  }

  private def publishTextField(d: Donation, f: String): String = {
    getTextFieldByName(d, f) map { v ⇒
      val label = f.head.toUpper + f.tail
      s"$label:\n  $v"
    } getOrElse ""
  }

  def cancelDonation(a: AgentPointer): Option[Donation] = FbState.finishDonation(a.node)

}
