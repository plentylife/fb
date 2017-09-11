package fb.donation

import plenty.state.model.Donation
import plenty.state.DonationStateUtils._
import DonationUtils._
import com.restfb.types.send.QuickReply
import com.restfb.types.webhook.messaging._
import fb.{FbAgent, FbState, Responses, Utility}
import plenty.agent.AgentPointer
import plenty.network.{DonateAction, Network}

object DonationFlow {

  /** field names ordered by the order of questions that the user is asked as they are filling out a new donation */
  private[donation] val fieldsInQuestionOrder: Seq[String] = Seq("title", "why", "what", "where", "when", "how", "who",
    "first_picture", "pictures")
  /** field names ordered by the order that they are presented in a post */
  private[donation] val fieldsInPostOrder: Seq[String] = Seq("what", "why", "where", "when", "how", "who")
  /** fields that are required to be filled out */
  private[donation] val requiredFields = Set("title", "what")


  /** Executes if there is a donation in progress (FbState has a reference)
    * Keeps asking questions until a new donation is finished.
    * @return true if the tree is executed */
  def flow(a: AgentPointer, msg: MessageItem): Boolean = {
    FbState.getDonation(a.node) match {
      case Some(d) ⇒
          flowCreateContinue(d, DonationMessage(msg), a)
          true
      case _ ⇒ false
    }
  }

  /** Executes if the postback is triggerting the creation of a new donation
    * Creates a new donation instance with empty fields and adds it to [[FbState]] to be tracked
    * @return `true` if the flow executes, `false` otherwise */
  def flow(a: AgentPointer, postback: PostbackItem): Boolean = {
    val payload = postback.getPayload
    if (payload != null && payload.nonEmpty) {
      if (payload == "DONATE_START_POSTBACK") {
        startDonationFlow(a)
        return true
      }
    }
    false
  }

  /** Listens to quick replies options in term (new donation cancelled, done)
    * @return true if triggered */
  def flow(a: AgentPointer, quickReply: QuickReplyItem): Boolean = {
    quickReply.getPayload match {
      case "DONATE_CANCEL_POSTBACK" =>
        cancelDonation(a)
        DonationResponses.donationCancelled(a.node.id)
        true
      case "DONATE_DONE_POSTBACK" =>
        FbState.finishDonation(a.node) match {
          case Some(d) ⇒
            println(s"finishing donation $d")
            publishDonation(d, a) match {
              case Some((pubDonation, postId)) =>
                println(s"published donation with post id $postId")
                val donationWithPostId = pubDonation.copy(id = postId)
                DonationResponses.showDonationBubble(a, donationWithPostId, Option(postId))
                Network.notifyAllAgents(donationWithPostId, DonateAction, FbAgent.node)
                DonationUtils.finalizeDonationPost(donationWithPostId)

              case _ => Responses.errorPersonal(a)
            }

          case _ ⇒
            Responses.errorPersonal(a, s"publishingPostDonePostback")
        }

        true
      case "DONATE_SKIP_POSTBACK" ⇒
        FbState.getDonation(a.node) match {
          case Some(d) ⇒
            flowCreateContinue(d, DonationMessage.empty, a)
            return true
          case _ ⇒ Responses.errorPersonal(a, "SkipDonationTag")
        }
        true
      case _ ⇒ false
    }
  }

  def startDonationFlow(a: AgentPointer) = {
    startDonation(a)
    DonationResponses.donationInstruction(a)
    DonationResponses.askNextQuestion(a, fieldsInQuestionOrder.head)
  }

  /** part of [[DonationFlow.flow()]] that deals with new donations in the process of their creation */
  private def flowCreateContinue(d: Donation, msg: DonationMessage, a: AgentPointer) = {
    // the question that the person should have just answered
    determineNextQuestion(d) match {
      case Some(thisQuestionField) ⇒
        val updated = fillOutDonationField(d, thisQuestionField, msg)
        updated foreach FbState.trackInProgress
        if ((thisQuestionField == "pictures" || thisQuestionField == "first_picture")
          && updated.isEmpty) DonationResponses.missingPicture(a.id)

        determineNextQuestion(updated.getOrElse(d)) match {
          case Some(nextQuestionField) ⇒
            val missedRequired = if (nextQuestionField == thisQuestionField &&
              requiredFields.contains(thisQuestionField)) true else false
            DonationResponses.askNextQuestion(a, nextQuestionField, missedRequired)

          // theoretically, should never be None
          case _ ⇒ Responses.errorPersonal(a, "flowCreateContinueIsNoneInner")
        }


      // theoretically, should never be None
      case _ ⇒ Responses.errorPersonal(a, "flowCreateContinueIsNoneOuter")
    }
  }


  /** using [[DonationFlow.fieldsInQuestionOrder]] finds a field that has not been filled yet, and returns the fields name
 * theoretically, should never return [[None]] because there could be unlimited number of pictures
    * @return None if all questions have been answered */
  private[donation] def determineNextQuestion(donation: Donation): Option[String] = {
    for (field ← fieldsInQuestionOrder) {
      if (!isFieldFilled(donation, field)) {
        return Some(field)
      }
    }

    return None
  }
}

/** This class is implemented to simplify some functions like skipping questions
  * In reality it just wraps [[MessageItem]] */
private[donation] trait DonationMessage {
  def text: Option[String] = None
  def isTextEmpty = text.getOrElse("").isEmpty
  def attachments: Seq[MessagingAttachment] = List.empty
}

private[donation] object DonationMessage {
  import scala.collection.JavaConverters

  def empty = new DonationMessage {}

  def apply(msg: MessageItem): DonationMessage = new DonationMessage {
    override def text: Option[String] = Option(msg.getText)
    override def attachments: Seq[MessagingAttachment] =
      JavaConverters.asScalaIterator(msg.getAttachments.iterator()).toSeq
  }
}