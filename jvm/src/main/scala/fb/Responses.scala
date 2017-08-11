package fb

import java.text.SimpleDateFormat
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Date

import com.restfb.types.send.Message
import com.restfb.types.send.IdMessageRecipient
import plenty.agent.{Accounting, AgentPointer}
import plenty.agent.model.Agent
import com.restfb.Parameter
import com.restfb.types.send.SendResponse

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

  def firstContact(agent: AgentPointer) = {
    val userInfo = UserInfo.get(agent.id)
    sendSimpleMessage(userInfo.id, s"Hey ${userInfo.name}!")
    accountStatus(agent)
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

}
