package fb

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
    Responder.displayTyping(senderId)
    getAgent(senderId) match {
      case Some(a) =>
        // todo remove
        val recipient = new IdMessageRecipient(senderId)
        val msg = new Message(s"Hey we know you!")
        fbClient.publish("me/messages", classOf[SendResponse],
          Parameter.`with`("recipient", recipient),
          Parameter.`with`("message", msg))
      case None =>
        Responder.firstContact(createAgent(senderId))

    }
  }

  private def getAgent(id: String): Option[AgentPointer] = {
    Network.getAgents.find(_.id == id)
  }

  private def createAgent(id: String): AgentPointer = {
    val a = AgentManager.createAgent(id)
    Network.registerAgent(a)
  }

}
