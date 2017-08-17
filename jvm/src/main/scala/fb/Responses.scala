package fb

import com.restfb.types.send.GenericTemplatePayload
import java.text.SimpleDateFormat
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Date

import com.restfb.types.send._
import plenty.agent.{Accounting, AgentPointer}
import plenty.agent.model.Agent
import com.restfb.Parameter
import com.restfb.types.{Comment, GraphResponse}
import com.restfb.types.webhook.FeedCommentValue
import plenty.state.model.{Bid, Donation, RejectedBid}

/**
  * Created by anton on 8/11/17.
  */
object Responses {

  def displayTyping(id: String) = {
    import com.restfb.Parameter
    import com.restfb.types.send.IdMessageRecipient
    import com.restfb.types.send.SendResponse
    import com.restfb.types.send.SenderActionEnum
    val recipient = new IdMessageRecipient(id)
    val senderActionParam = Parameter.`with`("sender_action", SenderActionEnum.typing_on)
    val recipientParam = Parameter.`with`("recipient", recipient)
    val resp = fbClient.publish("me/messages", classOf[SendResponse], senderActionParam, // the sender action
      recipientParam) // the recipient
  }

  private val dateFormatter = new SimpleDateFormat("dd MMM")
  private val thanksSymbol: Char = '\u20B8'

  def loginButton(senderId: String) = {
    val recipient = new IdMessageRecipient(senderId)
    val template = new ButtonTemplatePayload("to use Plenty bot, you must first link your FB account")
    val button = new AccountLinkButton(s"${Access.uri}/welcome/$senderId")
    template.addButton(button)
    fbClient.publish("me/messages", classOf[SendResponse], Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", new Message(new TemplateAttachment(template)))
    )
  }

  def accountStatus(agent: AgentPointer) = {
    val coins = agent.getAgentInLastKnownState.state.coins
    val coinsWithDeathDate = coins.toSeq sortBy (_.deathTime) map {c =>
      val d = new Date(c.deathTime)
      dateFormatter.format(d) -> c
    }
    val deathDateOredered = coinsWithDeathDate map (_._1)
    val coinsByDeathDate = coinsWithDeathDate groupBy (_._1)
    val coinCountByDeathDate = coinsByDeathDate map (c => c._1 -> c._2.size)
    val sortedBalance = coinCountByDeathDate.toSeq sortBy (c => deathDateOredered.indexOf(c._1))

    val expirationBlock = sortedBalance map {b => s"${b._2}$thanksSymbol expire on ${b._1}"} mkString "\n"
    val msg = s"Your account balance is ${coins.size} ${thanksSymbol}hanks:\n$expirationBlock"
    sendSimpleMessage(agent.id, msg)
  }

  def donationInstruction(agent: AgentPointer) = {
    val userInfo = UserInfo.get(agent.id)
    sendSimpleMessage(userInfo.id, s"${userInfo.name}, you have time, skills, or items to donate. The next few " +
      s"messages will be used to create the post. The first message will act as the title, and the rest as the body " +
      s"with text and pictures.")
    sendSimpleMessage(userInfo.id, "Please enter the title:")
  }

  def donationContinue(donation: Donation, agentPointer: AgentPointer) = {
    val recipient = new IdMessageRecipient(agentPointer.id)
    val hasTitle = donation.title.nonEmpty

    var txt = "You can add more information by sending text or images to Plenty, press `Done` to post, or `Cancel` to" +
      " discard."
    if (!hasTitle) {
      txt = "Please enter title, or press `Cancel` to discard"
    }

    val done = new QuickReply("Done", "DONATE_DONE_POSTBACK")
    val cancel = new QuickReply("Cancel", "DONATE_CANCEL_POSTBACK")
    val msg = new Message(txt)

    if (hasTitle) msg.addQuickReply(done)
    msg.addQuickReply(cancel)
    fbClient.publish("me/messages", classOf[SendResponse],
      Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", msg))
  }

  def donationCancelled(ui: UserInfo) = sendSimpleMessage(ui.id, "The donation was discarded")

  def donationShow(ui: UserInfo, donation: Donation, postId: Option[String]) = {
    val payload = new GenericTemplatePayload
    val bubble = new Bubble(s"Bid and Share: ${donation.title}")
    val bidButton = createBidButton(donation)
    val shareButton = new ShareButton()

    postId map {pid =>
      val url = s"https://www.facebook.com/$pid"
      val urlButton = new WebButton("View", url)
      bubble.addButton(urlButton)
    }

    bubble.addButton(bidButton)
    bubble.addButton(shareButton)
    bubble.setSubtitle(donation.description)
    payload.addBubble(bubble)

    val recipient = new IdMessageRecipient(ui.id)
    try {
      fbClient.publish("me/messages", classOf[SendResponse],
        Parameter.`with`("recipient", recipient),
        Parameter.`with`("message", new Message(new TemplateAttachment(payload))))
    } catch {
      case e: Throwable =>
        Responses.errorPersonal(ui)
        throw e
    }
  }
  private def createBidButton(donation: Donation) = new PostbackButton("Bid", s"BID_POSTBACK_${donation.id}")

  def bidStart(a: AgentPointer) = {
    accountStatus(a)
    sendSimpleMessage(a.id, s"How many ${thanksSymbol}hanks would you like to bid?")
  }

  def bidEntered(bid: Bid) = {
    val relatedBids = FbAgent.lastState.bids.filter(_.donation == bid.donation)
    val nodesToNotify = relatedBids map {_.by}
    nodesToNotify foreach {n =>
      // if the one who made the bid
      if (n == bid.by) {
        sendSimpleMessage(n.id, "Your bid has been entered. You'll be notified when the auction closes.")
      } else {
        // notify all other interested nodes
      val ui = UserInfo.get(n.id)
      val recipient = new IdMessageRecipient(n.id)
      val template = new ButtonTemplatePayload(s"A new bid of ${bid.amount}$thanksSymbol has been entered " +
        s"for ${bid.donation.title}")
      val button = createBidButton(bid.donation)
      template.addButton(button)
      fbClientPublish(ui, "me/messages", Parameter.`with`("recipient", recipient),
        Parameter.`with`("message", new Message(new TemplateAttachment(template))))
      }
    }
  }

  def bidRejected(rejection: RejectedBid, ui: UserInfo) = {
    sendSimpleMessage(rejection.bid.by.id, s"Your bid was rejected. Reason: ${rejection.reason}")
  }

  def firstContact(agent: AgentPointer) = {
    val userInfo = UserInfo.get(agent.id)
    sendSimpleMessage(userInfo.id, s"Hey ${userInfo.name}!")
    accountStatus(agent)
  }


  def errorPersonal(a: AgentPointer): Unit = {
    val ui = UserInfo.get(a.id)
    errorPersonal(ui)
  }
  def errorPersonal(ui: UserInfo): Unit = {
    val msg = {s"${ui.name}, sorry some unknown error has occurred. We will do our best to fix it."}
    sendSimpleMessage(ui.id, msg)
  }
  def errorWithReason(userId: String, reason: String) = {
    val msg = s"Sorry an error has occurred: $reason"
    sendSimpleMessage(userId, msg)
  }



  def unrecognized(a: AgentPointer) = {
    val ui = UserInfo.get(a.id)
    val msg = {s"${ui.name}, sorry we did not recognize this action"}
    sendSimpleMessage(ui.id, msg)
  }

  private def sendSimpleMessage(id: String, msg: String) = {
    val recipient = new IdMessageRecipient(id)
    fbClient.publish("me/messages", classOf[SendResponse],
      Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", new Message(msg)))
  }

  /** simple publish action with error catching (as user feedback) */
  private def fbClientPublish(ui: UserInfo, endpoint: String, parameters:Parameter*): GraphResponse = {
    try {
      fbClient.publish(endpoint, classOf[GraphResponse], parameters:_*)
    } catch {
      case e:Throwable =>
        errorPersonal(ui)
        throw e
    }
  }

}
