package fb.network

import java.util.logging.Logger

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import akka.http.scaladsl.server
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{MalformedQueryParamRejection, Rejection, Route}
import fb.Utility
import fb.donation.DonationFlow
import fb.network.FbWebviewUtils._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Encoder, ObjectEncoder}
import plenty.agent.AgentPointer
import plenty.state.StateCodecs._
import plenty.state.model.Node

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

private[network] object WebviewReceiver {
  val route: Route = pathPrefix("webview") {
    path("donation" / "fb" / Remaining) { id ⇒
      getPost(id) match {
        case Some(post) ⇒ complete(respond(post) toResponse)
        case None ⇒
          val rej: Rejection = MalformedQueryParamRejection("post id", "Failed to get the post data from FB")
          reject(rej)
      }
    } ~ WebviewSecurity.extractWithAgent() { case (msg, agentPointer) ⇒
      val agentIsNew = agentPointer.isEmpty

      if (agentIsNew) {
        // unsafe get, but this point should not be reachable if option is not full
        val apf = Utility.createAgent(Node(msg.userId.get))
        routeWithAgent(msg, apf)
      } else {
        routeWithAgent(msg, Future(agentPointer.get))
      }


    }
  }
  private[this] val logger = Logger.getLogger("Webview Receiver")
  private def routeWithAgent(msg: BaseMessage, apf: Future[AgentPointer]): server.Route = {
    pathPrefix("donation") {
      post {
        val r: Future[Response[_]] =
          apf map { ap ⇒
            msg.toTokens flatMap { tokenMsg ⇒
              DonationFlow.processDonationCreateRequest(tokenMsg.payload, ap.agentInLastState) map {
                d ⇒ respond(d)
              }
            } getOrElse error("")
          }

        complete(r map {_.toResponse})
      }
    }
  }

  private def respond[T: Encoder](payload: T): Response[T] = Response(isError = false, payload)
  private def error(reason: String): Response[String] = Response(isError = true, "")

}

private case class Response[T: Encoder](isError: Boolean, payload: T) {

  implicit private val encoder: ObjectEncoder[Response[T]] = deriveEncoder[Response[T]]

  implicit def toResponse: HttpResponse = {
    val s = if (isError) 400 else 200
    val e = HttpEntity.apply(this.asJson.noSpaces).withContentType(ContentTypes.`application/json`)
    HttpResponse.apply(status = s, entity = e)
  }
}