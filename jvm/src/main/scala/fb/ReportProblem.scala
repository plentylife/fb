package fb

import com.restfb.types.webhook.messaging.{MessageItem, QuickReplyItem}
import plenty.agent.AgentPointer
import plenty.state.model.Node

object ReportProblem {
  val POSTBACK_TAG = "REPORT_PROBLEM"
  val POSTBACK = s"${POSTBACK_TAG}_POSTBACK"
  val CANCEL_POSTBACK: String = Utility.getCancelQuickReplyPostback(POSTBACK_TAG)

  /** nodes that a reporting a problem */
  private var inProgress = Set[Node]()

  def flow(a: AgentPointer): PartialFunction[String, Unit] = {
    case POSTBACK ⇒
      synchronized {inProgress += a.node}
      R.prompt(a)
  }

  def flow(a: AgentPointer, quickReply: QuickReplyItem): Boolean = quickReply.getPayload match {
    case CANCEL_POSTBACK ⇒
      synchronized {inProgress -= a.node}
      true
    case _ ⇒ false
  }

  def flow(a: AgentPointer, msg: MessageItem): Boolean = {
    val exec = inProgress contains a.node
    if (exec) {
      U sendEmail msg.getText match {
        case 0 ⇒ R.success(a.node)
        case _ ⇒ R.failure(a.node)
      }
      synchronized {inProgress -= a.node}
    }
    exec
  }
}

private object R {
  type F = (Node) ⇒ Unit

  val prompt: (AgentPointer ⇒ Unit) =
    a ⇒ Responses.sendWithCancelOption(a, "What would you like to tell us?", ReportProblem.CANCEL_POSTBACK)
  val success: F = n ⇒ Responses.sendSimpleMessage(n.id, "We will get back to you shortly")
  val failure: F = n ⇒ Responses.sendSimpleMessage(n.id, "Sorry, we were unable to submit your feedback. Please email" +
    " us at admin@plenty.life")
}

private object U {
  def sendEmail(n: Node, msg: String): Int = {
    val body = s"${n.id} has feedback:\n\n$msg"
    sendEmail(body)
  }

  /** Sends out an email indicating a problem with user message to antonkats@gmail.com */
  def sendEmail(msg: String): Int = Utility.sendEmail(EmailInfo("Reported feedback in Plenty", body = msg, to =
    "antonkats@gmail.com"))
}