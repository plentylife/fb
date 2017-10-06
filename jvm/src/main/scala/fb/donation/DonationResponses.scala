package fb.donation

import com.restfb.Parameter
import com.restfb.types.send._
import fb.Responses.{createBidButton, sendSimpleMessage}
import fb._
import plenty.agent.AgentPointer
import plenty.state.StateManager
import plenty.state.model.{BidTransaction, Donation}

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
    "first_picture" → "Also, people love *pictures*. Send us a few. When finished, press `Done`.",
    "pictures" → "Perhaps another picture? When finished, press `Done`."
  )

  /** which questions can be the last question asked*/
  private val canFinishOnQuestion = Set("first_picture", "pictures")

  private val isRequiredText = "(required)"
  private val canBeBlankText = "(can be blank)"
  private val missedRequiredText = "this answer cannot be blank"

  /** maximum length of the first line of subtitle in donation template in characters */
  private val subtitleWhatMaxLength = 100
  private val subtitleWhereMaxLength = 15

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
    sendWithOptions(a, fullText, canFinishOnQuestion contains questionName, required)
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
    if (!required && !canFinish) msg.addQuickReply(skip)
    msg.addQuickReply(cancel)
    Responses.fbClientPublish(a, "me/messages",
      Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", msg))
  }

  def donationCancelled(id: String) = sendSimpleMessage(id, "The donation was discarded")

  def missingPicture(id: String) = sendSimpleMessage(id, "That wasn't a picture")

  def showDonationBubble(a: AgentPointer, donation: Donation, postId: Option[String],
                         biddingMode: Boolean = false) = {
    ???
    val payload = new GenericTemplatePayload
    val bubble = new Bubble(s"Bid and Share: missing title")
    val localBidButton = createBidButton(donation)
    val remoteBidButton = new WebButton("Bid", s"m.me/${FbSettings.pageId}?ref=BID_${donation.id}")
    val shareButton = new ShareButton()

    // images
    // fixme the images come from a CDN and are temporary
    // that means that they soon disappear from the bubble
    // for now, removing
//    donation.attachments.headOption foreach bubble.setImageUrl
    // view link
    postId foreach {pid =>
      bubble.addButton(donationLinkButton(pid))
    }
    // bid button
    if (!biddingMode)
      bubble.addButton(remoteBidButton)
    else bubble.addButton(localBidButton)
    // share button
    if (postId.nonEmpty) bubble.addButton(shareButton)
    payload.addBubble(bubble)

    //    var subtitle = donation.what.getOrElse("")
    var subtitle = ""
    if (subtitle.length > subtitleWhatMaxLength) subtitle = subtitle.take(subtitleWhatMaxLength) + "..."
    //    donation.where foreach {where ⇒
    //      val subWhere = if (where.length > subtitleWhereMaxLength) where.take(subtitleWhereMaxLength) + "..." else
    // where
    //      subtitle += s"\n$subWhere"
    //    }

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

  /** creates a [[WebButton]] that links to the given post with title view */
  private def donationLinkButton(postId: String, title: String = "View") = {
    val url = s"https://www.facebook.com/$postId"
    new WebButton(title, url)
  }

  /** sends a message to all bidders, and the donor when a bid wins */
  def donationSettled(fromTransaction: BidTransaction) = {
    // fixme
    ???
    val fromBid = fromTransaction.bid
    val relatedBids = StateManager.getRelatedBids(FbAgent.lastState, fromBid)
    relatedBids foreach { relBid ⇒
      val ui = UserInfo.get(relBid.by.id)
      //      val title = fromBid.donation.title.getOrElse("missing title")
      val title = "fixme"
      if (relBid.by == fromTransaction.from) {
        sendSimpleMessage(ui.id, s"You have WON the auction for `$title`!")
        askToLeaveContact(ui, fromBid.donation, "This will allow the donor to contact you")
      } else {
        sendSimpleMessage(ui.id, s"You have LOST the auction for `$title`")
      }
    }
    // notifying the donor
    val donation = fromBid.donation
    val donor = donation.by.id
    // fixme
    //    sendSimpleMessage(donor, s"The auction for '${donation.title.getOrElse("missing title")}' has closed with
    // the " +
    sendSimpleMessage(donor, s"The auction for has closed with the " +
      s"highest bid of ${fromTransaction.coins.size}")
    askToLeaveContact(UserInfo.get(donor), fromBid.donation, "This will allow the auction winner to contact you")
  }

  /** sends a message asking to leave contact information */
  def askToLeaveContact(userInfo: UserInfo, donation: Donation, explanation: String): Unit = {
    val template = new ButtonTemplatePayload(s"Please leave your contact information in the comments. $explanation")
    val button = donationLinkButton(donation.id, "Leave comment")
    val reportBtn = new PostbackButton("Report a problem", ReportProblem.POSTBACK)
    template.addButton(button)
    template.addButton(reportBtn)
    val msg = new Message(new TemplateAttachment(template))
    Responses.fbClientPublish(userInfo, "me/messages",
      Parameter.`with`("message", msg), Parameter.`with`("recipient", new IdMessageRecipient(userInfo.id))
    )
  }

}
