package fb

import java.text.SimpleDateFormat
import java.util.logging.Logger

import com.restfb.Parameter
import com.restfb.types.GraphResponse
import com.restfb.types.send._
import fb.network.FbWebviewUtils
import plenty.agent.model.Agent
import plenty.agent.{Accounting, AgentPointer}
import plenty.state.model.{Bid, Donation, RejectedBid}

/**
  * Created by anton on 8/11/17.
  */
object Responses {
  private val logger = Logger.getLogger("Responses")

  def displayTyping(id: String) = {
    import com.restfb.Parameter
    import com.restfb.types.send.{IdMessageRecipient, SendResponse, SenderActionEnum}
    val recipient = new IdMessageRecipient(id)
    val senderActionParam = Parameter.`with`("sender_action", SenderActionEnum.typing_on)
    val recipientParam = Parameter.`with`("recipient", recipient)
    val resp = fbClient.publish("me/messages", classOf[SendResponse], senderActionParam, // the sender action
      recipientParam) // the recipient
  }

  private val dateFormatter = new SimpleDateFormat("dd MMM")

  def accountStatus(agent: AgentPointer) = {
    val balance = Accounting.getSelfBalance(agent.agentInLastState)
    val rate = Accounting.calculateDemurageRate(agent.agentInLastState)
    val expirationBlock = s"expiring at ${Math.ceil(rate * 100)}% per day"

    val msg = s"Your account balance is ${balance} ${thanksSymbol}hanks:\n$expirationBlock"
    sendSimpleMessage(agent.id, msg)
  }

  def updateToAccountBalance(a: AgentPointer, byHowMuch: Int, comment: String = ""): Unit = {
    val amount = if (byHowMuch > 0) s"increased by +$byHowMuch" else s"decreased by -$byHowMuch"
    if (byHowMuch == 0) {
      return
    }
    val msg = s"Your account balance $amount"
    sendSimpleMessage(a.id, msg)
  }

  def updateToAccountBalance(aId: String, byHowMuch: Int, comment: String): Unit = {
    val amount = if (byHowMuch > 0) s"increased by +$byHowMuch" else s"decreased by -$byHowMuch"
    if (byHowMuch == 0) {
      return
    }
    val msg = s"Your account balance $amount $comment"
    sendSimpleMessage(aId, msg)
  }

  def bidEntered(bid: Bid) = {
    // since this bid is not yet in the state
    val relatedBids = (FbAgent.lastState.bids + bid).filter(_.donation == bid.donation)
    val nodesToNotify = relatedBids map {_.by}
    nodesToNotify foreach {n =>
      // if the one who made the bid
      // todo no need to send this anymore
      if (n == bid.by) {
        // sendSimpleMessage(n.id, "Your bid has been entered. You'll be notified when the auction closes.")
      } else {
        // notify all other interested nodes
      val ui = UserInfo.get(n.id)
      val recipient = new IdMessageRecipient(n.id)
        val template = new ButtonTemplatePayload(s"A new bid of ${bid.amount}$thanksSymbol has been entered" +
          s". Up your bid to win")
        //        s"for ${bid.donation.title.getOrElse("missing title")}. Up your bid to win")
      val button = createBidButton(bid.donation)
      template.addButton(button)
      fbClientPublish(ui, "me/messages", Parameter.`with`("recipient", recipient),
        Parameter.`with`("message", new Message(new TemplateAttachment(template))))
      }
    }

    // allowing the donor to accept a bid early
    val donation = bid.donation
    val donor = donation.by.id
    val ui = UserInfo.get(donor)
    val recipient = new IdMessageRecipient(donor)
    val template = new ButtonTemplatePayload(s"A new bid of ${bid.amount}$thanksSymbol has been entered " +
      s". You can " +
      //      s"for '${bid.donation.title.getOrElse("missing title")}'. The highest bid is ${maxBid}${thanksSymbol}. You can " +
        "close the auction now or wait until it closes automatically")
    val button = new PostbackButton("Close auction", s"BID_ACCEPT_POSTBACK_${donation.id}")
    val viewButton = FbWebviewUtils.setButtonAsWebview(
      new WebButton("View offer", FbWebviewUtils.makeUriGlobal(FbWebviewUtils.viewDonationUrl(donation)))
    )

    template.addButton(viewButton)
    template.addButton(button)
    fbClientPublish(ui, "me/messages", Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", new Message(new TemplateAttachment(template))))
  }
  def createBidButton(donation: Donation) = {
    val b = FbWebviewUtils.setButtonAsWebview(
      new WebButton("up your Bid", FbWebviewUtils.makeUriGlobal(FbWebviewUtils.viewDonationUrl
      (donation))))
    b.setMessengerExtensions(true, null)
    b
  }
  def bidStart(a: AgentPointer) = {
    accountStatus(a)
    sendWithCancelOption(a, s"How many ${thanksSymbol}hanks would you like to bid? (a round number)",
      "CANCEL_BID_POSTBACK")
  }

  def bidRejected(rejection: RejectedBid, ui: UserInfo) = {
    sendWithCancelOption(ui, s"Your bid was rejected. Reason: ${rejection.reason}. Try again", "CANCEL_BID_POSTBACK")
  }

  def firstContact(agent: AgentPointer) = {
    val userInfo = UserInfo.get(agent.id)
    sendSimpleMessage(userInfo.id, s"Hey ${userInfo.name}!")
    //    sendSimpleMessage(userInfo.id,
    //      "Plenty is an auction with a kick: money that you earn expires." +
    //        "Don't worry though, you can't set a price on your items, so use Plenty to get rid of stuff you don't
    // want" +
    //        " anyways")

    sendIntroInfoButton(userInfo)
  }

  /** sends a brief message with a button link to the page */
  def sendIntroInfoButton(userInfo: UserInfo): Unit = {
    val recipient = new IdMessageRecipient(userInfo.id)
    val template = new ButtonTemplatePayload("Plenty is simple to use. To open it in Messenger, use the " +
      "action menu icon by the message composer at the bottom of the screen. Start by Searching for items, or " +
      "Donating unused stuff.")
    val db = new WebButton("Donate", FbWebviewUtils.makeUriGlobal(FbWebviewUtils.createDonationUrl))
    db.setMessengerExtensions(true, null)
    //    db.setMessengerExtensions(true, FbWebviewUtils.makeUriGlobal(FbWebviewUtils.fallbackPageUrl))
    db.setWebviewHeightRatio(WebviewHeightEnum.full)
    template.addButton(db)
    val sb = new WebButton("Search", FbWebviewUtils.makeUriGlobal(FbWebviewUtils.searchDonationUrl))
    sb.setMessengerExtensions(true, null)
    //    sb.setMessengerExtensions(true, FbWebviewUtils.makeUriGlobal(FbWebviewUtils.fallbackPageUrl))
    db.setWebviewHeightRatio(WebviewHeightEnum.full)
    template.addButton(sb)
    fbClientPublish(userInfo, "me/messages", Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", new Message(new TemplateAttachment(template))))
  }

  def notifyOfComment(d: Donation, comment: String, commentId: String) = {
    val msg = s"New comment: `$comment`\n(to reply, make sure your are logged in to Facebook)"
    val r = new IdMessageRecipient(d.by.id)
    val t = new ButtonTemplatePayload(msg)
    val b = new WebButton("Reply", s"https://www.facebook.com/$commentId")
    t.addButton(b)
    try {
      fbClient.publish("me/messages", classOf[GraphResponse],
        Parameter.`with`("recipient", r),
        Parameter.`with`("message", new Message(new TemplateAttachment(t))))
    } catch {
      case e: Throwable ⇒ logger.fine(s"Could not send out a message notifying of a comment. " +
        s"Donation $d, comment id $commentId, error ${e.getMessage}")
    }
  }

  /** sends a message asking if the user would like to make a donation */
  def sendDonateButton(userInfo: UserInfo): Unit = {
    val recipient = new IdMessageRecipient(userInfo.id)
    val template = new ButtonTemplatePayload(s"Make your first donation, ${userInfo.name}! It can be time, skills, " +
      s"knowledge, physical items, or anything in-between")
    val button = new PostbackButton("Donate", "DONATE_START_POSTBACK")
    template.addButton(button)
    fbClientPublish(userInfo, "me/messages", Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", new Message(new TemplateAttachment(template))))
  }

  def errorPersonal(a: AgentPointer, errorTag: String): Unit =
    errorPersonal(a.agentInLastState, errorTag)
  def errorPersonal(a: Agent, errorTag: String = ""): Unit = {
    val ui = UserInfo.get(a.id)
    errorPersonal(ui, errorTag)
  }
  def errorPersonal(ui: UserInfo, errorTag: String): Unit = {
    val msg = {s"${ui.name}, sorry some unknown error has occurred. We will do our best to fix it."}
    sendSimpleMessage(ui.id, msg)
    println(s"ERROR with errorPersonal(): tag $errorTag")
  }
  def errorWithReason(userId: String, reason: String) = {
    val msg = s"Sorry an error has occurred: $reason"
    sendSimpleMessage(userId, msg)
  }

  def unrecognizedAction(a: AgentPointer) = {
    val ui = UserInfo.get(a.id)
    val msg = {s"${ui.name}, sorry we did not recognize this action"}
    sendSimpleMessage(ui.id, msg)
  }

  def sendSimpleMessage(id: String, msg: String) = {
    val recipient = new IdMessageRecipient(id)
    fbClient.publish("me/messages", classOf[SendResponse],
      Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", new Message(msg)))
  }


  def sendLink(msg: String, url: String, linkTitle: String, toId: String): Unit = {
    try {
      val recipient = new IdMessageRecipient(toId)
      val template = new ButtonTemplatePayload(msg)
      val button = new WebButton(linkTitle, url)
      template.addButton(button)
      fbClient.publish("me/messages", classOf[SendResponse],
        Parameter.`with`("recipient", recipient),
        Parameter.`with`("message", new Message(new TemplateAttachment(template))))
    } catch {
      case e: Throwable ⇒ logger.warning(s"Could not send link. ${e.getMessage}")
    }
  }

  /** Sends a message with a quick option to cancel */
  def sendWithCancelOption(a: AgentPointer, text: String, postbackTag: String): Unit = {
    sendWithCancelOption(UserInfo.get(a.id), text, postbackTag)
  }

  def sendWithCancelOption(ui: UserInfo, text: String, postbackTag: String): Unit = {
    val recipient = new IdMessageRecipient(ui.id)

    val cancel = new QuickReply("Cancel", postbackTag)
    val msg = new Message(text)

    msg.addQuickReply(cancel)
    Responses.fbClientPublish(ui, "me/messages",
      Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", msg))
  }

  /** simple publish action with error catching (as user feedback) */
  def fbClientPublish(a: AgentPointer, endpoint: String, parameters:Parameter*): GraphResponse = {
    fbClientPublish(UserInfo.get(a.id), endpoint, parameters:_*)
  }

  /** simple publish action with error catching (as user feedback) */
  // todo add option for adding recipient from ui
  def fbClientPublish(ui: UserInfo, endpoint: String, parameters:Parameter*): GraphResponse = {
    try {
      fbClient.publish(endpoint, classOf[GraphResponse], parameters:_*)
    } catch {
      case e:Throwable =>
        errorPersonal(ui, s"ClientPublishOnEndpoint: $endpoint")
        throw e
    }
  }

}
