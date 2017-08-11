package fb

import java.util.Date

import com.restfb.{DefaultJsonMapper, JsonMapper, Parameter}
import com.restfb.types.send.{IdMessageRecipient, Message, SendResponse}
import com.restfb.types.webhook.messaging.MessagingItem
import com.restfb.types.webhook.{WebhookEntry, WebhookObject}
import plenty.agent.{AgentManager, AgentPointer}
import plenty.agent.model.Agent
import plenty.network.Network

/**
  * Created by anton on 8/10/17.
  */
object ReceiverFlow {

  def receive(payload: String): Unit = {
    val mapper = new DefaultJsonMapper
    val webhookObject = mapper.toJavaObject(payload, classOf[WebhookObject])
    val iter = webhookObject.getEntryList.listIterator()
    while (iter.hasNext) {
      process(iter.next())
    }
  }

  private def process(entry: WebhookEntry) = {
    println("Message Entry")
    println(entry.toString)

    val iter = entry.getMessaging.iterator()
    while (iter.hasNext) {
      processMessagingItem(iter.next())
    }
  }

  private def processMessagingItem(item: MessagingItem) = {
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
      pb.getPayload match {
        case "ACCOUNT_STATUS_POSTBACK" => Responses.accountStatus(a)
        case _ => Responses.unrecognized(a)
      }
    }
  }

  private def messageTree(a: AgentPointer, item: MessagingItem) = {
    val msg = item.getMessage
    println(s"message is $msg")
    if (msg != null) {
      val ui = UserInfo.get(a.id)
      // hasn't accessed in a day
      if (ui.lastAccess < new Date().getTime - 24 * 60 * 60 * 1000) {
        Responses.accountStatus(a)
      } else {
        val recipient = new IdMessageRecipient(ui.id)
        val msg = new Message(s"We were just talking!")
        fbClient.publish("me/messages", classOf[SendResponse],
          Parameter.`with`("recipient", recipient),
          Parameter.`with`("message", msg))
      }
    }
  }

//  private def agentExistsTree(a: AgentPointer) = {
//    messageTree()
//  }

  private def getAgent(id: String): Option[AgentPointer] = {
    Network.getAgents.find(_.id == id)
  }

  private def createAgent(id: String): AgentPointer = {
    val a = AgentManager.createAgent(id)
    Network.registerAgent(a)
  }

}
