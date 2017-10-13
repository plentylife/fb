package fb.donation

import java.util
import java.util.logging._

import com.restfb.Parameter
import com.restfb.types.{GraphResponse, Post}
import fb._
import plenty.agent.AgentPointer
import plenty.agent.model.Agent
import plenty.state.DonationStateUtils._
import plenty.state.StateManager
import plenty.state.model.{Bid, Donation}

private[donation] object DonationUtils {
  private val logger = Logger.getLogger("DonationUtils")

  def startDonation(a: AgentPointer): Donation = {
    val d = StateManager.createEmptyDonation(a.node)
    FbState.trackInProgress(d)
    d
  }

  def fillOutDonationField(d: Donation, field: String, msg: DonationMessage): Option[Donation] = {
    if (field == "pictures" || field == "first_picture") {
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
    ???
    //    val attachments = msg.attachments
    //    val pictures = attachments filter (_.getType == "image") map (_.getPayload.getUrl)
    //
    //    if (pictures.nonEmpty)
    //      Option(d.copy(attachments = d.attachments ++ pictures))
    //    else None
  }

  /**
    * Finishes the donation, drops it from [[FbState]] in-progress tacking, and publishes it as a post on fb
    *
    * @return donation with post id
    **/
  def publishDonation(donation: Donation, a: Agent): Option[(Donation, String)] = {
    try {
      val msg = producePostBody(donation)
      val publishMessageResponse = fbClient.publish(s"${FbSettings.pageId}/feed",
        classOf[Post],
        Parameter.`with`("message", msg)
      )
      Some(donation -> publishMessageResponse.getId)
    } catch {
      case e: Throwable =>
        Responses.errorPersonal(a, s"publishingPostInUtils $e")
        None
    }
  }

  /** runs the current post message through a supplied function, and updates the donation post with the result.
    * assumes that the donation id is the post id. */
  private[donation] def updateDonation(donation: Donation, updateBy: (String, Donation) ⇒ String): Unit = {
    try {
      val fieldList = new util.ArrayList[String]()
      fieldList.add("message")
      fieldList.add("id")
      val params = Parameter.`with`("fields", fieldList)
      val post = fbClient.fetchObject(s"${donation.id}", classOf[Post], params)
      val currentMessage = post.getMessage
      val updatedMessage = updateBy(currentMessage, donation)
      val resp = fbClient.publish(s"${donation.id}", classOf[GraphResponse],
        Parameter.`with`("message", updatedMessage))
      assert(resp.isSuccess, "fb graph response was not success")
    } catch {
      case e: Throwable =>
        logger.log(Level.WARNING, s"failed to update donation post. Error ${e.getMessage}")
        throw e
    }
  }
  /** @return a string with the questions and answers -- the bulk of the donation description in a post */
  private def producePostBody(d: Donation): String = {
    d.description map { t ⇒
      if (t.isTagged) s"\t__${t.token}__\t" else t.token
    } mkString " "
  }
  /** updates donation post with action links */
  def finalizeDonationPost(donation: Donation): Unit = {
    val updater = (oldMessage: String, d: Donation) ⇒ {
      val openLink = s"m.me/${FbSettings.pageId}?ref=OPEN_${donation.id}"
//      val shortBidLink = Utility.getShortLink(bidLink)
val bidBlock = s"\n===\nTo bid (buy) follow $openLink\nOpens in Facebook messenger"
      oldMessage + bidBlock
    }
    updateDonation(donation, updater)
  }

  private def publishTextField(d: Donation, f: String): String = {
    getTextFieldByName(d, f) map { v ⇒
      val label = f.head.toUpper + f.tail
      s"$label:\n  $v"
    } getOrElse ""
  }

  def cancelDonation(a: AgentPointer): Option[Donation] = FbState.finishDonation(a.node)
}

private[fb] object ExternalDonationUtils {
  /** puts a header on the donation post
    * assumes that the donation id is post id */
  def markPostAsSettled(winningBid: Bid): Unit = {
    val updater = (oldMessage: String, d: Donation) ⇒ {
      val header = s"This auction is CLOSED. The winning bid amount is ${winningBid.amount}$thanksSymbol\n===\n\n"
      header + oldMessage
    }
    DonationUtils.updateDonation(winningBid.donation, updater)
  }
}
