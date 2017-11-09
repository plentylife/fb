package fb.network

import java.util.logging.Logger

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{MalformedQueryParamRejection, Rejection, Route}
import fb._
import fb.donation.DonationFlow
import fb.network.FbWebviewUtils._
import fb.network.PlentyWebviewUtils.{amountDecoder, _}
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Encoder, ObjectEncoder}
import plenty.agent.AgentPointer
import plenty.state.Search
import plenty.state.StateCodecs._
import plenty.state.model.{Donation, Node}

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
    } ~ (path("search") & parameter("q")) { q ⇒
      var search: List[Donation] = List()
      if (q.trim().isEmpty) {
        search = FbAgent.lastState.donations.toSeq.sortBy(_.timestamp).takeRight(10).toList
      } else {
        search = Search.searchDescriptionsByString(q, FbAgent.lastState)
      }
      complete(respond(search).toResponse)
    } ~ WebviewSecurity.extractWithAgent() { case (msg, agentPointer) ⇒
      val agentIsNew = agentPointer.isEmpty

      if (agentIsNew) {
        // unsafe get, but this point should not be reachable if option is not full
        val apf = Utility.createAgent(Node(msg.userId.get))
        apf map Responses.firstContact
        routeWithAgent(msg, apf, agentIsNew)
      } else {
        routeWithAgent(msg, Future(agentPointer.get), agentIsNew)
      }


    }
  }
  private[this] val logger = Logger.getLogger("Webview Receiver")
  private def routeWithAgent(msg: BaseMessage, apf: Future[AgentPointer], agentIsNew: Boolean): server.Route = {
    pathPrefix("donation") {
      (path(Segment) & parameterMap) { (id, params) ⇒
        put {
          val respFuture = apf map { ap ⇒
            msg.payload.as[Amount] match {
              case Left(e) ⇒ error("the number was likely not an integer")
              case Right(amount) ⇒
                PlentyWebviewUtils.bid(ap, id, amount.amount, params.get("ref")) match {
                  case Some(reason) ⇒ error(reason)
                  case None ⇒
                    respond("")
                }
            }
          }

          complete(respFuture map {_.toResponse})
        }
      } ~ post {
        pathPrefix(Segment) { id ⇒
          path("highest_bid") {
            val r: Future[Response[_]] = apf map { ap ⇒
              val bid = highestCurrentBid(ap, id)
              respond(bid map attachInfo)
            }

            complete(r map {_.toResponse})

          } ~ path("user") {
            val resp = PlentyWebviewUtils.donationUserInfo(id) match {
              case Some(ui) ⇒ respond(ui)
              case None ⇒ error("could not get information about the user who made the donation")
            }

            complete(resp.toResponse)
          }

        } ~ pathEndOrSingleSlash {
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
    } ~ (path("account") & post) {
      val r = apf map { ap ⇒
        val s = accountStatus(ap)
        respond(s) toResponse
      }
      complete(r)
    } ~ path("heartbeat") {
      val resp = apf map { ap ⇒
        val ui = UserInfo.get(ap.id)
        if (ui.hasFailed) HttpResponse.apply(status = StatusCodes.BadRequest)
        else {
          var r = Response(isError = false, ui).toResponse
          if (agentIsNew) r = r.copy(status = StatusCodes.NotFound)
          r
        }
      }
      complete(resp)
    }
  }

  private def respond[T: Encoder](payload: T): Response[T] = Response(isError = false, payload)
  private def error(reason: String): Response[String] = Response(isError = true, reason)

}

private case class Response[T: Encoder](isError: Boolean, payload: T) {

  implicit private val encoder: ObjectEncoder[Response[T]] = deriveEncoder[Response[T]]

  implicit def toResponse: HttpResponse = {
    val s = if (isError) 400 else 200
    val e = HttpEntity.apply(this.asJson.noSpaces).withContentType(ContentTypes.`application/json`)
    HttpResponse.apply(status = s, entity = e)
  }
}