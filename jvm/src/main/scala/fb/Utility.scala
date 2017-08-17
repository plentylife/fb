package fb

import java.util

import com.restfb.Parameter
import com.restfb.types.webhook.FeedCommentValue
import com.restfb.types.webhook.messaging.MessagingItem
import com.restfb.types.{Photo, Post}
import plenty.agent.{AgentManager, AgentPointer}
import plenty.network.{BidAction, DonateAction, Network, Message => NetMessage}
import plenty.state.StateManager
import plenty.state.model.{Donation, Node}

import scala.collection.JavaConverters
import scala.language.postfixOps

/**
  * Created by anton on 8/15/17.
  */
object Utility {

  def startDonationProcess(a: AgentPointer) = {
    FbState.getOrCreateDonation(AgentManager.agentAsNode(a.getAgentInLastKnownState))
  }

  /** gets the node of the sender from FB agent [[plenty.state.model.State]] */
  def getNodeFromFbAgent(msg: MessagingItem): Option[Node] = {
    val byId = msg.getSender.getId
    FbAgent.pointer.getAgentInLastKnownState.state.nodes.find(_.id == byId)
  }

  def updateDonation(msg: MessagingItem, node: Node): Donation = {
    var d: Donation = FbState.getOrCreateDonation(node)

    val attachments = JavaConverters.asScalaBuffer(msg.getMessage.getAttachments)
    val pictures = attachments filter (_.getType == "image") map (_.getPayload.getUrl)
    d = d.copy(attachments = d.attachments ++ pictures)

    val text = msg.getMessage.getText
    if (text != null) {
      if (d.title.isEmpty) {
        val ui = UserInfo.get(node.id)
        d = d.copy(title = text + s" donated by ${ui.name}")
      } else {
        val sep = if (d.description.isEmpty) "" else "\n"
        d = d.copy(description = d.description + s"$sep$text")
      }
    }

    FbState.update(d)
    d
  }

  /** @return post id */
  def publishDonation(a: AgentPointer): Option[(Donation, String)] = FbState.finishDonation(a.node) match {
    case Some(donation: Donation) =>
      // fixme don't allow of posting of empty title/description
      val attachments = new util.ArrayList[String]()
      donation.attachments map { url =>
        val id = fbClient.publish(s"${Access.pageId}/photos",
          classOf[Photo], Parameter.`with`("url", url), Parameter.`with`("published", false)
        ).getId
        s"{'media_fbid':'${id}'}"
      } foreach {
        attachments.add
      }
      try {
        val msg = s"${donation.title} \n---\n ${donation.description} " +
          s"\n===\n if you want this offer, enter your bid at m.me/${Access
          .pageId}?ref=BID_${donation.id}\nthe link opens messenger and allows you to talk to Plenty bot"
        val publishMessageResponse = fbClient.publish(s"${Access.pageId}/feed",
          classOf[Post],
          Parameter.`with`("message", msg),
          Parameter.`with`("attached_media", attachments)
        )
        Network.notifyAllAgents(donation, DonateAction, FbAgent.node)
        Some(donation -> publishMessageResponse.getId)
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

  def startBidding(donationRef: String, a: AgentPointer) = {
    val donationId = donationRef.replace("BID_POSTBACK_", "")
    FbAgent.lastState.donations.find(_.id == donationId) match {
      case Some(donation) => FbState.trackBid(a, donation)
      case _ => Responses.errorWithReason(a.id, "Could not find the donation")
    }
  }

  private val bidRegex = "[0-9]+".r

  def processTextAsBid(txt: String, donation: Donation, a: AgentPointer) = {
    bidRegex.findFirstMatchIn(txt) match {
      case Some(rm) =>
        val amount = rm.group(0).toInt
        val bid = StateManager.createBid(donation, amount, a.node)
        println(s"bid ${bid}")
        Network.notifyAllAgents(bid, BidAction, from=a.node)
      case _ => Responses.errorPersonal(a)
    }
  }
}
