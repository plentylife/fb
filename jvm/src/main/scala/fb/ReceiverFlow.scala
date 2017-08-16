package fb

import java.util.Date

import com.restfb.types.send.{IdMessageRecipient, Message, SendResponse}
import com.restfb.types.webhook.messaging.MessagingItem
import com.restfb.types.webhook.{WebhookEntry, WebhookObject}
import com.restfb.{DefaultJsonMapper, Parameter}
import plenty.agent.{AgentManager, AgentPointer}
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
    println(s"webhook object $webhookObject")
    while (iter.hasNext) {
      process(iter.next())
    }
  }

  private def process(entry: WebhookEntry) = {
    val iter = entry.getMessaging.iterator()
    while (iter.hasNext) {
      processMessagingItem(iter.next())
    }
  }

  private def processMessagingItem(item: MessagingItem) = {
//    println(s"Messaging item $item")
    val senderId = item.getSender.getId
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
  }

  private def postbackTree(a: AgentPointer, item: MessagingItem) = {
    val pb = item.getPostback
    println(s"postback is $pb")

    if (pb != null) {
      val ui = UserInfo.get(a.id)
      pb.getPayload match {
        case "ACCOUNT_STATUS_POSTBACK" => Responses.accountStatus(a)
        case "DONATE_START_POSTBACK" =>
          Utility.startDonationProcess(a)
          Responses.donationInstruction(a)
        case _ => Responses.unrecognized(a)
      }
    }
  }

  private def messageTree(a: AgentPointer, msgItem: MessagingItem) = {
    val msg = msgItem.getMessage
    //    println(s"message is $msg")
    if (msg != null) {
      val ui = UserInfo.get(a.id)

      val isQuickReply = quickReplyTree(msgItem, ui, a)
      var isDonating = false
      if (!isQuickReply) {
        isDonating = donationTree(a, msgItem)
      }

      if (!isDonating && !isQuickReply) {
        // hasn't accessed in a day
        if (ui.lastAccess < new Date().getTime - 24 * 60 * 60 * 1000) {
          Responses.accountStatus(a)
        } else {
          val recipient = new IdMessageRecipient(ui.id)
          val msg = new Message(s"We were just talking!")
          fbMsgClient.publish("me/messages", classOf[SendResponse],
            Parameter.`with`("recipient", recipient),
            Parameter.`with`("message", msg))
        }
      }

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
            case Some((donation, postId)) => Responses.donationDone(ui, donation, postId)
            case _ => Responses.errorPersonal(a)
          }
      }
      true
    } else false
  }

  /** @return true if the tree is executed */
  private def donationTree(a: AgentPointer, msgItem: MessagingItem): Boolean = {
    if (FbState.donationExists(a.node)) {

      Utility.getNodeFromNetwork(msgItem) match {
        case Some(node) =>
          Utility.updateDonation(msgItem, node)
          Responses.donationContinue(a)
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
    val a = AgentManager.createAgent(id)
    val n = AgentManager.agentAsNode(a)
    FbAgent.registerNode(n)
    Network.registerAgent(a)
  }

}
