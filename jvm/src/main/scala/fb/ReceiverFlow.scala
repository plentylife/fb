package fb

import com.restfb.types.send.{IdMessageRecipient, Message, SendResponse}
import com.restfb.types.webhook.messaging.{MessageItem, MessagingItem, QuickReplyItem}
import com.restfb.types.webhook.{WebhookEntry, WebhookObject}
import com.restfb.{DefaultJsonMapper, Parameter}
import fb.donation.{DonationFlow, DonationResponses}
import fb.network.CommentWatcher
import plenty.agent.AgentPointer
import plenty.agent.logic.AgentActions
import plenty.executionContext
import plenty.state.model.Node

import scala.concurrent.Future

/**
  * Created by anton on 8/10/17.
  */
object ReceiverFlow {

  def receive(payload: String): Unit = {
    println(s"payload $payload")
    val mapper = new DefaultJsonMapper
    val webhookObject = mapper.toJavaObject(payload, classOf[WebhookObject])
    val iter = webhookObject.getEntryList.listIterator()
    while (iter.hasNext) {
      process(iter.next())
    }
  }

  private def process(entry: WebhookEntry): Unit = {
    CommentWatcher.receiveWebhook(entry)
    val msgIter = entry.getMessaging.iterator()
    while (msgIter.hasNext) {
      processMessagingItem(msgIter.next())
    }
  }

  private def processMessagingItem(item: MessagingItem): Unit = {
    val senderId = item.getSender.getId
    // todo figure out if this is needed
//    Responses.displayTyping(senderId)

    var needsInto = false
    (getAgent(senderId) match {
      case Some(a) =>
        messageTree(a, item)
        Future {a}
      case None =>
        needsInto = true
        Utility.createAgent(Node(senderId))
    }) foreach { a ⇒
      val postback = postbackTree(a, item)
      val biddingCallback: Option[() ⇒ Unit] = msgReferralTree(a, item)
      val isBidding = biddingCallback.nonEmpty

      if (needsInto && postback != "GET_STARTED_PAYLOAD") Responses.firstContact(a)
      // this is for formatting purposes. The bid action should come after into
      biddingCallback foreach (f ⇒ f())
    }
  }

  private def messageTree(a: AgentPointer, msgItem: MessagingItem): Unit = {
    val msg = msgItem.getMessage
    println(s"message is $msg")
    if (msg != null) {
      val ui = UserInfo.get(a.id)

      val isQuickReply = quickReplyTree(msgItem, ui, a)
      var isDonating = false
      if (!isQuickReply) {
        isDonating = DonationFlow.flow(a, msg)
      }
      val isBidding = bidTree(a, msg)
      val isReporting = ReportProblem.flow(a, msg)

      if (!isDonating && !isQuickReply && !isBidding && !isReporting) {
        if (msg.getText != null && msg.getText.trim.toLowerCase() == "donate") {
          DonationFlow.startDonationFlow(a)
        } else if (isAskingForBalance(msg)) {
          Responses.accountStatus(a)
        } else {
          val recipient = new IdMessageRecipient(ui.id)
          val msg = new Message(s"We are not sure what you mean. Use the menu beside the message compose bar, at the " +
            s"bottom of the screen")
          fbClient.publish("me/messages", classOf[SendResponse],
            Parameter.`with`("recipient", recipient),
            Parameter.`with`("message", msg))
        }
      }

    }
  }

  private def isAskingForBalance(msg: MessageItem) = {
    msg.getText != null && {
      val t = msg.getText.toLowerCase
      (t contains "thank") || (t contains "balance") || (t contains "status")
    }
  }

  /**
    * @return the postback payload (text) */
  private def postbackTree(a: AgentPointer, item: MessagingItem): String = {
    val pb = item.getPostback
    println(s"postback is $pb")

    if (pb != null) {
      val ui = UserInfo.get(a.id)
      val unallocatedCases: PartialFunction[String, Unit] = {
        case "GET_STARTED_PAYLOAD" ⇒
          Responses.firstContact(a)
          if (pb.getReferral != null) {
            processRefString(pb.getReferral.getRef, a)
          }
        case "ACCOUNT_STATUS_POSTBACK" => Responses.accountStatus(a)
        case p: String if p.startsWith("BID_POSTBACK_") =>
          val bidPossible = Utility.startBidding(p, a)
          if (bidPossible) Responses.bidStart(a)
        case p: String if p.startsWith("BID_ACCEPT_POSTBACK_") =>
          AgentActions.takeBids(a.agentInLastState, hardAuctionClose = true)

      }

      val fullTree = unallocatedCases orElse DonationFlow.flow(a) orElse ReportProblem.flow(a) orElse ({
        case _ ⇒ Responses.unrecognizedAction(a)
      }: PartialFunction[String, Unit])
      fullTree(pb.getPayload)

      pb.getPayload
    } else ""
  }

  /** @return `true` if there is a ref (for now means bidding) */
  private def msgReferralTree(a: AgentPointer, item: MessagingItem): Option[() ⇒ Unit] = {
    if (item.getReferral != null) {
      val ref = item.getReferral.getRef
      val callback = () ⇒ processRefString(ref, a)
      return Some(callback)
    }
    None
  }

  private val quickReplyTreeFlows = Set[Function2[AgentPointer, QuickReplyItem, Boolean]](DonationFlow.flow,
    ReportProblem.flow, bidQuickReplyFlow)
  /** @return true if the tree is executed */
  private def quickReplyTree(msgItem: MessagingItem, ui: UserInfo, a: AgentPointer): Boolean = {
    val qr = msgItem.getMessage.getQuickReply
    if (qr != null) {
      var triggeredAny = false
      quickReplyTreeFlows.takeWhile { f ⇒
        val didTrigger = f(a, qr)
        triggeredAny = didTrigger
        // reversing, since the idea is to keep trying until one succeeds
        !didTrigger
      }
      triggeredAny
    } else false
  }

  private def bidQuickReplyFlow(a: AgentPointer, qr: QuickReplyItem): Boolean = {
    qr.getPayload match {
      case "CANCEL_BID_POSTBACK" ⇒
        FbState.popBid(a)
        true
      case _ ⇒ false
    }
  }

  private def processRefString(ref: String, a: AgentPointer): Unit = {
    if (ref.startsWith("OPEN_")) {
      val donationId = ref.replace("OPEN_", "")
      FbAgent.lastState.donations.find(_.id == donationId) match {
        case Some(donation) => DonationResponses.showDonationBubble(a, donation, None, biddingMode = true)
        case _ =>
          println("ERROR " + s"processRefStringBid ${Option(ref).getOrElse("null ref")}")
          Responses.errorWithReason(a.id, "the auction seems to have been closed")
      }
    }
  }

  /* bidding and donations */

  private def bidTree(a: AgentPointer, msg: MessageItem): Boolean = {
    FbState.popBid(a) match {
      case Some(d) =>
        if (Utility.processTextAsBid(msg.getText, d, a)) {

        } else {
          FbState.trackBid(a, d)
          Responses.sendWithCancelOption(a, "Was that a round number? Try again", "CANCEL_BID_POSTBACK")
        }
        true
      case _ => false
    }
  }

  //  private def agentExistsTree(a: AgentPointer) = {
  //    messageTree()
  //  }

  def getAgent(id: String): Option[AgentPointer] = Utility.getAgent(id)


}
