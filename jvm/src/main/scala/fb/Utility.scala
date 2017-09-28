package fb

import java.util.logging.Logger

import akka.http.scaladsl.model._
import akka.util.ByteString
import com.restfb.types.webhook.messaging.MessagingItem
import plenty.agent.AgentPointer
import plenty.network.{ActionIdentifiers, BidAction, Network}
import plenty.state.StateManager
import plenty.state.model.{Donation, Node}
import plenty.{agent, executionContext}

import scala.concurrent.Future
import scala.language.postfixOps
import scala.sys.process._
import scala.util.parsing.json.{JSON, JSONObject}

/**
  * Created by anton on 8/15/17.
  */
object Utility {
  private val logger = Logger.getLogger("Fb Utility")

  def createAgent(n: Node): Future[AgentPointer] = {
    val a = agent.createAgent(n, FbAgent.lastState)

    FbAgent.registerNode(n)
    val p = Network.registerAgent(a, FbSendReceiveInterface)
    Network.notifyAllAgents(n, ActionIdentifiers.REGISTER_NODE, FbAgent.node)
    CoinDistributor.give(p) map { _ ⇒ p }
  }

  /** gets the node of the sender from FB agent [[plenty.state.model.State]] */
  def getNodeFromFbAgent(msg: MessagingItem): Option[Node] = {
    val byId = msg.getSender.getId
    FbAgent.pointer.agentInLastState.state.nodes.find(_.id == byId)
  }

  /** @return `true` if a bid can be made */
  def startBidding(donationRef: String, a: AgentPointer): Boolean = {
    val donationId = donationRef.replace("BID_POSTBACK_", "")
    FbAgent.lastState.donations.find(_.id == donationId) match {
      case Some(donation) => FbState.trackBid(a, donation); true
      case _ => Responses.errorWithReason(a.id, "Could not find the donation"); false
    }
  }

  private val bidRegex = "[0-9]+".r

  def processTextAsBid(txt: String, donation: Donation, a: AgentPointer): Boolean = {
    bidRegex.findFirstMatchIn(txt) match {
      case Some(rm) =>
        val amountStr = rm.group(0)
        // making sure it was not a float
        if (amountStr.trim.length != txt.trim.length) {
          return false
        }

        val amount = amountStr.toInt
        val bid = StateManager.createBid(donation, amount, a.node)
        logger.finer(s"Created bid $bid")
        Network.notifyAllAgents(bid, BidAction, from = a.node)
        true
      case _ =>
        false
    }
  }

  /** Sends an email using sendmail */
  def sendEmail(e: EmailInfo): Int = {
    val cmd = s"echo '${e.body}' | mail -s '${e.subject}' ${e.to}"
    Process(Seq("sh", "-c", cmd)).!
  }

  /** takes the link through google's shortner service
    *
    * @return a future with the new link */
  def getShortLink(url: String): String = {
    println("getting short link")
    val reqUrl = s"https://www.googleapis.com/urlshortener/v1/url?key=${FbSettings.googleShortnerApiKey}"
    val reqBody = ByteString(s"{'longUrl': '$url'}")
    val entity = HttpEntity.apply(ContentTypes.`application/json`, reqBody)
    val req = HttpRequest(method = HttpMethods.POST, entity = entity, uri = reqUrl)

    val data = FbServer.makeRequest(req)

    println("parsing into JSON")
    val jsonObject = JSON.parseRaw(data.toString) collect { case json: JSONObject ⇒ json }
    println("parsed into JSON")
    val shortUrl = jsonObject.map({_.obj.getOrElse("id", "").toString})
    shortUrl.getOrElse("")
  }

  def getCancelQuickReplyPostback(tag: String): String = tag + "_CANCEL_POSTBACK"
}

case class EmailInfo(subject: String, body: String, to: String)
