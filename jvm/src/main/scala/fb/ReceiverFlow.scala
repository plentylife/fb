package fb

import java.util.Date

import com.restfb.types.send.{IdMessageRecipient, Message, SendResponse}
import com.restfb.types.webhook.messaging.{MessageItem, MessagingItem, QuickReplyItem}
import com.restfb.types.webhook.{Change, FeedCommentValue, WebhookEntry, WebhookObject}
import com.restfb.{DefaultJsonMapper, Parameter}
import fb.donation.{DonationFlow, DonationResponses}
import plenty.agent.{AgentManager, AgentPointer, StateLogic}
import plenty.network.Network

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

  private def process(entry: WebhookEntry) = {
    val msgIter = entry.getMessaging.iterator()
    while (msgIter.hasNext) {
      processMessagingItem(msgIter.next())
    }
  }

  private def processMessagingItem(item: MessagingItem) = {
    val senderId = item.getSender.getId
//    println(s"Messaging item $item")
    Responses.displayTyping(senderId)

    var a: AgentPointer = null
    var needsInto = false
    getAgent(senderId) match {
      case Some(_a) =>
        a = _a
        messageTree(a, item)
      case None =>
        a = createAgent(senderId)
        needsInto = true
//        Responses.firstContact(a)
    }
    val postback = postbackTree(a, item)
    msgReferralTree(a, item)

    if (needsInto && postback != "GET_STARTED_PAYLOAD") Responses.firstContact(a)
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

      if (!isDonating && !isQuickReply && !isBidding) {
        if (msg.getText != null && msg.getText.trim.toLowerCase() == "donate") {
          DonationFlow.startDonationFlow(a)
        } else {
          val recipient = new IdMessageRecipient(ui.id)
          val msg = new Message(s"We are not sure what you mean")
          fbClient.publish("me/messages", classOf[SendResponse],
            Parameter.`with`("recipient", recipient),
            Parameter.`with`("message", msg))
        }
      }

    }
  }

  /**
    * @return the postback payload (text) */
  private def postbackTree(a: AgentPointer, item: MessagingItem): String = {
    val pb = item.getPostback
    println(s"postback is $pb")

    if (pb != null) {
      val ui = UserInfo.get(a.id)
      pb.getPayload match {
        case "GET_STARTED_PAYLOAD" ⇒
          Responses.firstContact(a, isBidding = pb.getReferral != null)
          if (pb.getReferral != null) {
            processRefString(pb.getReferral.getRef, a)
          }
        case "ACCOUNT_STATUS_POSTBACK" => Responses.accountStatus(a)
        case p: String if p.startsWith("BID_POSTBACK_") =>
          val bidPossible = Utility.startBidding(p,a)
          if (bidPossible) Responses.bidStart(a)
        case p: String if p.startsWith("BID_ACCEPT_POSTBACK_") =>
          AgentManager.takeBids(a.getAgentInLastKnownState, hardAuctionClose = true)
        case _ =>
          // checking for donation postback
          val donationPostback = DonationFlow.flow(a, pb)
          if (!donationPostback) Responses.unrecognizedAction(a)
      }
      pb.getPayload
    } else ""
  }

  private def msgReferralTree(a: AgentPointer, item: MessagingItem) = {
    if (item.getReferral != null) {
      val ref = item.getReferral.getRef
      processRefString(ref, a)
    }
  }

  private val quickReplyTreeFlows = Set[Function2[AgentPointer, QuickReplyItem, Boolean]](DonationFlow.flow)
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

  private def processRefString(ref: String, a: AgentPointer) = {
    if (ref.startsWith("BID_")) {
      val donationId = ref.replace("BID_", "")
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
        Utility.processTextAsBid(msg.getText, d, a)
        true
      case _ => false
    }
  }

  //  private def agentExistsTree(a: AgentPointer) = {
  //    messageTree()
  //  }

  private def getAgent(id: String): Option[AgentPointer] = {
    Network.getAgents.find(_.id == id)
  }

  private def createAgent(id: String): AgentPointer = {
    var a = AgentManager.createAgent(id, FbAgent.lastState)
    // adding other coins already existent in network
//    a = StateLogic.registerCoins(FbAgent.lastState.coins, a)
//    // as well as donations and bids
//    var s = a.state
//    s = s.copy(donations = s.donations ++ FbAgent.lastState.donations)
//    a = a.copy(state = s)

    val n = AgentManager.agentAsNode(a)
    FbAgent.registerNode(n)
    Network.registerAgent(a, FbSendInterface)
  }

}
