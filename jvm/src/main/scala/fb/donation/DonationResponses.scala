package fb.donation

import com.restfb.Parameter
import com.restfb.types.send._
import fb.Responses.{createBidButton, sendSimpleMessage}
import fb._
import plenty.agent.AgentPointer
import plenty.state.StateManager
import plenty.state.model.{Donation, Transaction}
import Responses._

object DonationResponses {

  /** a map of question name to question text, used for interacting with user while collecting info about a
    * new donation */
  private val questionText = Map(
    "title" → "What is the attention grabber? Just a few words please, this will be the *title*",
    "why" → "*Why* are you making this donation?",
    "what" → "Describe *what* it is. Is it your skills, time or a physical item?",
    "when" → "Are there time limits? You can meet only at certain times? *When*, how long, which days, etc",
    "where" → "*Where*? What is the location?",
    "how" → "*How* can this be obtained? Are there conditions, etc?",
    "who" → "People like to know *who* they are interacting with. A few words about yourself, please.",
    "pictures" → "Also, people love *pictures*. Send us a few. When finished, press `Done`."
  )

  private val isRequiredText = "(required)"
  private val canBeBlankText = "(can be blank)"
  private val missedRequiredText = "this answer cannot be blank"

  /** maximum length of the first line of subtitle in donation template in characters */
  private val subtitleWhatMaxLenght = 100
  private val subtitleWhereMaxLenght = 15

  /** sending appropriate response to the user, prompting to enter the next piece of info.
    * if a field was missed (in last answer), notifies the user of the fact
    * gives quickreply options for `cancel` and `done` (if the last question is reached) */
  def askNextQuestion(a: AgentPointer, questionName: String, missedLastTime: Boolean = false): Unit
  = {
    val baseText = questionText(questionName)
    val requiredText = if (DonationFlow.requiredFields.contains(questionName)) {
      isRequiredText
    } else canBeBlankText
    val fullText = baseText + " " + requiredText

    if (missedLastTime) sendSimpleMessage(a.id, missedRequiredText)
    val required = DonationFlow.requiredFields.contains(questionName)
    sendWithOptions(a, fullText, DonationFlow.fieldsInQuestionOrder.last == questionName, required)
  }

  def donationInstruction(agent: AgentPointer) = {
    val userInfo = UserInfo.get(agent.id)
    sendSimpleMessage(userInfo.id, s"${userInfo.name}, let's have a conversation about what you are donating")
    sendSimpleMessage(userInfo.id, s"we'll ask you a few simple questions, with some suggestions, feel free to " +
      s"elaborate")
  }

  /** Sends a message with quick responses (done, cancel, and skip)*/
  def sendWithOptions(a: AgentPointer, text: String, canFinish: Boolean, required: Boolean) = {
    val recipient = new IdMessageRecipient(a.id)

    val done = new QuickReply("Done", "DONATE_DONE_POSTBACK")
    val skip = new QuickReply("Skip", "DONATE_SKIP_POSTBACK")
    val cancel = new QuickReply("Cancel", "DONATE_CANCEL_POSTBACK")
    val msg = new Message(text)

    if (canFinish) msg.addQuickReply(done)
    if (!required) msg.addQuickReply(skip)
    msg.addQuickReply(cancel)
    Responses.fbClientPublish(a, "me/messages",
      Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", msg))
  }

  def donationCancelled(id: String) = sendSimpleMessage(id, "The donation was discarded")

  def missingPicture(id: String) = sendSimpleMessage(id, "That wasn't a picture")

  def showDonationBubble(a: AgentPointer, donation: Donation, postId: Option[String],
                         showBidButton: Boolean = true) = {
    val payload = new GenericTemplatePayload
    val bubble = new Bubble(s"Bid and Share: ${donation.title}")
    val bidButton =
      if (showBidButton) createBidButton(donation)
      else new WebButton("Bid", s"m.me/${FbSettings.pageId}?ref=BID_${donation.id}")
    val shareButton = new ShareButton()

    postId foreach {pid =>
      val url = s"https://www.facebook.com/$pid"
      val urlButton = new WebButton("View", url)
      bubble.addButton(urlButton)
    }
    bubble.addButton(bidButton)
    if (postId.nonEmpty) bubble.addButton(shareButton)
    payload.addBubble(bubble)

    var subtitle = donation.what.getOrElse("")
    if (subtitle.length > subtitleWhatMaxLenght) subtitle = subtitle.take(subtitleWhatMaxLenght) + "..."
    donation.where foreach {where ⇒
      val subWhere = if (where.length > subtitleWhereMaxLenght) where.take(subtitleWhereMaxLenght) + "..." else where
      subtitle += s"\n$subWhere"
    }

    bubble.setSubtitle(subtitle)

    val recipient = new IdMessageRecipient(a.id)
    try {
      fbClient.publish("me/messages", classOf[SendResponse],
        Parameter.`with`("recipient", recipient),
        Parameter.`with`("message", new Message(new TemplateAttachment(payload))))
    } catch {
      case e: Throwable =>
        Responses.errorPersonal(a, errorTag = "DonationShowTag")
        throw e
    }
  }

  def donationSettled(fromTransaction: Transaction) = {
    val fromBid = fromTransaction.bid.get
    StateManager.getRelatedBids(FbAgent.lastState, fromBid) foreach { relBid ⇒
      val ui = UserInfo.get(relBid.by.id)
      if (relBid.by == fromTransaction.from) {
        sendSimpleMessage(ui.id, s"Your have WON the auction for '${fromBid.donation.title}'!")
      } else {
        sendSimpleMessage(ui.id, s"Your have LOST the auction for '${fromBid.donation.title}'")
      }
    }
    // notifying the donor
    val donation = fromBid.donation
    val donor = donation.by.id
    sendSimpleMessage(donor, s"The auction for '${donation.title}' has closed with the highest bid of " +
      s"${fromTransaction.coins.size}")
  }

}
