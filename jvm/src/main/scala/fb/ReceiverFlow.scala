package fb

import java.util.Date

import com.restfb.types.send.{IdMessageRecipient, Message, SendResponse}
import com.restfb.types.webhook.messaging.{MessageItem, MessagingItem}
import com.restfb.types.webhook.{Change, FeedCommentValue, WebhookEntry, WebhookObject}
import com.restfb.{DefaultJsonMapper, Parameter}
import plenty.agent.{AgentManager, AgentPointer, StateLogic}
import plenty.network.Network

/**
  * Created by anton on 8/10/17.
  */
object ReceiverFlow {

  def receive(payload: String): Unit = {
//    println(s"payload $payload")
    val mapper = new DefaultJsonMapper
    val webhookObject = mapper.toJavaObject(payload, classOf[WebhookObject])
    val iter = webhookObject.getEntryList.listIterator()
//    println(s"webhook object $webhookObject")
    while (iter.hasNext) {
      process(iter.next())
    }
  }

  private def process(entry: WebhookEntry) = {
    val msgIter = entry.getMessaging.iterator()
    val feedIter = entry.getChanges.iterator()
    while (msgIter.hasNext) {
      processMessagingItem(msgIter.next())
    }
  }

  private def processMessagingItem(item: MessagingItem) = {
    val senderId = item.getSender.getId
//    println(s"Messaging item $item")
    Responses.displayTyping(senderId)

    var a: AgentPointer = null
    getAgent(senderId) match {
      case Some(_a) =>
        a = _a
        messageTree(a, item)
      //        agentExistsTree(a)
      case None =>
        a = createAgent(senderId)
        Responses.firstContact(a)
    }
    postbackTree(a, item)
    msgReferralTree(a, item)
  }

  private def msgReferralTree(a: AgentPointer, item: MessagingItem) = {
    if (item.getReferral != null) {
      val ref = item.getReferral.getRef
      processRefString(ref, a)
    }
  }

  private def processRefString(ref: String, a: AgentPointer) = {
    if (ref.startsWith("BID_")) {
      val donationId = ref.replace("BID_", "")
      val ui = UserInfo.get(a.id)
      FbAgent.lastState.donations.find(_.id == donationId) match {
        case Some(donation) => Responses.donationShow(ui, donation, None, postbackBid = true, showShare = false)
        case _ => Responses.errorPersonal(a)
      }
    }
  }

  private def postbackTree(a: AgentPointer, item: MessagingItem) = {
    val pb = item.getPostback
    println(s"postback is $pb")

    if (pb != null) {
      val ui = UserInfo.get(a.id)
      pb.getPayload match {
        case "GET_STARTED_PAYLOAD" if pb.getReferral != null ⇒
          processRefString(pb.getReferral.getRef, a)
        case "ACCOUNT_STATUS_POSTBACK" => Responses.accountStatus(a)
        case "DONATE_START_POSTBACK" =>
          donateStart(a)
        case p: String if p.startsWith("BID_POSTBACK_") =>
          Utility.startBidding(p,a)
          Responses.bidStart(a)
        case p: String if p.startsWith("BID_ACCEPT_POSTBACK_") =>
          AgentManager.takeBids(a.getAgentInLastKnownState, hardAuctionClose = true)
        case _ => Responses.unrecognized(a)
      }
    }
  }

  private def donateStart(a: AgentPointer) = {
    Utility.startDonationProcess(a)
    Responses.donationInstruction(a)
  }

  private def messageTree(a: AgentPointer, msgItem: MessagingItem): Unit = {
    val msg = msgItem.getMessage
        println(s"message is $msg")
    if (msg != null) {
      val ui = UserInfo.get(a.id)

      val isQuickReply = quickReplyTree(msgItem, ui, a)
      var isDonating = false
      if (!isQuickReply) {
        isDonating = donationTree(a, msgItem)
      }
      val isBidding = bidTree(a, msg)

      if (!isDonating && !isQuickReply && !isBidding) {
        if (msg.getText.trim.toLowerCase() == "donate") {
          donateStart(a)
        } else if (ui.lastAccess < new Date().getTime - 24 * 60 * 60 * 1000) {
          // hasn't accessed in a day
          Responses.accountStatus(a)
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

  private def bidTree(a: AgentPointer, msg: MessageItem): Boolean = {
    FbState.popBid(a) match {
      case Some(d) =>
        Utility.processTextAsBid(msg.getText, d, a)
        true
      case _ => false
    }
  }

  /** @return true if the tree is executed */
  private def quickReplyTree(msgItem: MessagingItem, ui: UserInfo, a: AgentPointer): Boolean = {
    val qr = msgItem.getMessage.getQuickReply
    if (qr != null) {
      qr.getPayload match {
        case "DONATE_CANCEL_POSTBACK" =>
          Utility.cancelDonation(a)
          Responses.donationCancelled(ui)
        case "DONATE_DONE_POSTBACK" =>
          Utility.publishDonation(a) match {
            case Some((donation, postId)) => Responses.donationShow(ui, donation, Option(postId), postbackBid = false)
            case _ => Responses.errorPersonal(a)
          }
      }
      true
    } else false
  }

  /** Executes if there is a donation in progress (FbState has a reference)
    * @return true if the tree is executed */
  private def donationTree(a: AgentPointer, msgItem: MessagingItem): Boolean = {
    if (FbState.donationExists(a.node)) {
      Utility.getNodeFromFbAgent(msgItem) match {
        case Some(node) =>
          val d = Utility.updateDonation(msgItem, node)
          Responses.donationContinue(d, a)
        case _ => Responses.errorPersonal(a)
      }
      true
    } else false
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
