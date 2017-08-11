package fb

import com.restfb.types.send.Message
import com.restfb.types.send.IdMessageRecipient
import plenty.agent.AgentPointer
import plenty.agent.model.Agent
import com.restfb.Parameter
import com.restfb.types.send.SendResponse

/**
  * Created by anton on 8/11/17.
  */
object Responder {

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

  def firstContact(agent: AgentPointer) = {
    val userInfo = UserInfo.get(agent.id)
    val recipient = new IdMessageRecipient(userInfo.id)
    val msg = new Message(s"Hey ${userInfo.name}!")
    fbClient.publish("me/messages", classOf[SendResponse],
      Parameter.`with`("recipient", recipient),
      Parameter.`with`("message", msg))
  }

}
