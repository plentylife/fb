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
import plenty.state.model.{Bid, Donation}

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
    sendSimpleMessage(userInfo.id, s"${userInfo.name}, you have time, skills, or items to donate. In your next " +
      s"message describe those: for example, 'Math tutoring for one hour' or 'Ice cream maker' with pictures attached")
  }

  def donationContinue(agentPointer: AgentPointer) = {
    val txt = "You can add more information by sending text or images to Plenty, press `Done` to post, or `Cancel` to" +
      " discard."
    val recipient = new IdMessageRecipient(agentPointer.id)

    val done = new QuickReply("Done", "DONATE_DONE_POSTBACK")
    val cancel = new QuickReply("Cancel", "DONATE_CANCEL_POSTBACK")
    val msg = new Message(txt)
    msg.addQuickReply(done)
    msg.addQuickReply(cancel)
    fbClient.publish("me/messages", classOf[SendResponse],
      Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", msg))
  }

  def donationCancelled(ui: UserInfo) = sendSimpleMessage(ui.id, "The donation was discarded")

  def donationDone(ui: UserInfo, donation: Donation, postId: String) = {
    val url = s"https://www.facebook.com/$postId"
    val payload = new GenericTemplatePayload
    val bubble = new Bubble(s"${ui.name} wants you to share and bid on")
    val urlButton = new WebButton("View & Bid", url)
    val shareButton = new ShareButton()
    bubble.addButton(urlButton)
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

  def bidEntered(bid: Bid, replyToCommentId: String, userInfo: UserInfo) = {
    try {
      fbClient.publish(s"$replyToCommentId/comments", classOf[GraphResponse],
        Parameter.`with`("message", s"Your bid of ${bid.amount}$thanksSymbol has been entered"))
    } catch {
      case e:Throwable =>
        errorPersonal(userInfo)
        throw e
    }
  }

//  def bidRejected(bid: )

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
