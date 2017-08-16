package fb

import java.util

import com.restfb.{BinaryAttachment, Parameter}
import com.restfb.types.{GraphResponse, Photo, Post}
import com.restfb.types.send.{MediaAttachment, Message}
import com.restfb.types.webhook.FeedCommentValue
import com.restfb.types.webhook.messaging.{MessagingAttachment, MessagingItem}
import plenty.agent.{AgentManager, AgentPointer}
import plenty.agent.model.Agent
import plenty.network.Network
import plenty.network.communication.DonateAction
import plenty.state.StateManager
import plenty.state.model.{Donation, Node}

import scala.collection.{JavaConversions, JavaConverters}
import scala.language.postfixOps

/**
  * Created by anton on 8/15/17.
  */
object Utility {

  def startDonationProcess(a: AgentPointer) = {
    FbState.getOrCreateDonation(AgentManager.agentAsNode(a.getAgentInLastKnownState))
  }

  def getNodeFromNetwork(msg: MessagingItem): Option[Node] = {
    val byId = msg.getSender.getId
    FbAgent.pointer.getAgentInLastKnownState.state.nodes.find(_.id == byId)
  }

  def updateDonation(msg: MessagingItem, node: Node): Unit = {
    var d: Donation = FbState.getOrCreateDonation(node)

    val attachments = JavaConverters.asScalaBuffer(msg.getMessage.getAttachments)
    val pictures = attachments filter(_.getType == "image") map(_.getPayload.getUrl)
    val description = msg.getMessage.getText

    d = d.copy(attachments = d.attachments ++ pictures)
    if (description != null) {
      val sep = if (d.description.isEmpty) "" else "\n"
      d = d.copy(description = d.description + s"$sep$description")
    }

    println(s"description ${d.description}")

    FbState.update(d)
  }

  /** @return post id*/
  def publishDonation(a: AgentPointer): Option[(Donation, String)] = FbState.finishDonation(a.node) match {
    case Some(donation: Donation) =>
      val attachments = new util.ArrayList[String]()
      donation.attachments map {url =>
        val id = fbMsgClient.publish(s"${Access.pageId}/photos",
          classOf[Photo], Parameter.`with`("url", url), Parameter.`with`("published", false)
        ).getId
        s"{'media_fbid':'${id}'}"
      } foreach {attachments.add}
      try {
        val publishMessageResponse = fbMsgClient.publish(s"${Access.pageId}/feed",
          classOf[Post],
          Parameter.`with`("message", donation.description),
          Parameter.`with`("attached_media", attachments)
        )
        // modifying donation to have id same as post id
        val dWithPostId = donation.copy(id = publishMessageResponse.getId)
        Network.notifyAllAgents(dWithPostId, DonateAction, FbAgent.node)
        Some(dWithPostId -> publishMessageResponse.getId)
      } catch {
        case e: Throwable =>
          Responses.errorPersonal(a)
          throw e
      }
    case _ =>
      Responses.errorPersonal(a)
      None
  }

  def cancelDonation(a: AgentPointer) = FbState.finishDonation(a.node)

  private val bidRegex = "bid ([0-9]+)".r
  def processCommentAsBid(comment: FeedCommentValue, a: AgentPointer) = {
    val txt = comment.getMessage.trim.toLowerCase
    bidRegex.findFirstMatchIn(txt) match {
      case Some(rm) =>
        val amount = rm.group(1).toInt
        val postId = comment.getPostId
        a.getAgentInLastKnownState.state.donations.find(_.id == postId) match {
          case Some(d) =>
            val bid = StateManager.createBid(d, amount, a.node)
            println(s"bid ${bid}")
            Responses.bidEntered(bid, comment, a)
          case _ => Responses.errorPersonal(a)
        }
      case _ =>
    }

  }
}
