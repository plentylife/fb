package fb

import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers._
import akka.http.scaladsl.unmarshalling._
import akka.util.ByteString
import com.restfb.types.webhook.messaging.MessagingItem
import plenty.agent.AgentPointer
import plenty.network.{BidAction, Network, Message ⇒ NetMessage}
import plenty.state.StateManager
import plenty.state.model.{Donation, Node}

import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.parsing.json.{JSON, JSONObject}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by anton on 8/15/17.
  */
object Utility {

  /** gets the node of the sender from FB agent [[plenty.state.model.State]] */
  def getNodeFromFbAgent(msg: MessagingItem): Option[Node] = {
    val byId = msg.getSender.getId
    FbAgent.pointer.getAgentInLastKnownState.state.nodes.find(_.id == byId)
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

  def processTextAsBid(txt: String, donation: Donation, a: AgentPointer) = {
    bidRegex.findFirstMatchIn(txt) match {
      case Some(rm) =>
        val amount = rm.group(0).toInt
        val bid = StateManager.createBid(donation, amount, a.node)
        println(s"bid ${bid}")
        Network.notifyAllAgents(bid, BidAction, from = a.node)
      case _ => Responses.errorWithReason(a.id, "perhaps that wasn't a number. Try pressing `bid` again and entering " +
        s"an amount of ${thanksSymbol}hanks")
    }
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
    val jsonObject = JSON.parseRaw(data.toString()) collect { case json: JSONObject ⇒ json }
    println("parsed into JSON")
    val shortUrl = jsonObject.map({_.obj.getOrElse("id", "").toString})
    shortUrl.getOrElse("")
  }
}
