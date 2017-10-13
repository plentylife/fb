package fb.network

import java.util.Base64
import java.util.logging.Logger

import akka.http.scaladsl.server.Directives.{as, entity, reject}
import akka.http.scaladsl.server.directives.BasicDirectives.{provide, tprovide}
import akka.http.scaladsl.server.{Directive, Directive1, ValidationRejection}
import fb.{FbSettings, Utility}
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.decode
import plenty.agent.AgentPointer

private[network] trait SecureMessage {

  import javax.crypto.Mac
  import javax.crypto.spec.SecretKeySpec

  val signedRequest: String
  private val keySpec = new SecretKeySpec(FbSettings.appSecret.getBytes, "HmacSHA256")
  private val mac: Mac = Mac.getInstance("HmacSHA256")

  def userId: Option[String] = {
    if (isSecure) {
      decodePayload.map(_.psid)
    } else {
      None
    }
  }

  def isSecure: Boolean = {
    val (sig64, payload) = splitSignedRequest
    val sig = Base64.getUrlDecoder.decode(sig64).toList
    val expected = mac.doFinal(payload.getBytes()).toList
    sig == expected
  }

  private def splitSignedRequest = {
    val s = signedRequest.split('.')
    (s(0), s(1))
  }

  /** Allows to get the usedId */
  private def decodePayload = {
    val (_, p) = splitAndDecodeSignedRequest
    decode[SecurePayload](p.map(_.toChar).mkString).toOption
  }

  mac.init(keySpec)

  private def splitAndDecodeSignedRequest = {
    val (sig64, p64) = splitSignedRequest
    val sig = Base64.getUrlDecoder.decode(sig64)
    val p = Base64.getUrlDecoder.decode(p64)
    (sig, p)
  }
}

private case class SecurePayload(psid: String)

private[network] object WebviewSecurity {
  protected[this] val logger: Logger = Logger.getLogger("WebviewSecurity")

  implicit val msgDecoder: Decoder[BaseMessage] = deriveDecoder[BaseMessage]

  /** Passes on the message only if the payload is successfully decoded, and the security of the message is validated */
  def extractMessage(): Directive1[BaseMessage] = {
    entity(as[String]) flatMap { str ⇒
      logger.finer(s"webview raw message: $str")
      decode[BaseMessage](str) fold( { e ⇒
        logger.fine(s"Webview could not decode message: $e")
        reject(ValidationRejection("Could not process the body", Option(e))).toDirective[Tuple1[BaseMessage]]
      }, { msg ⇒
        if (msg.isSecure) provide(msg) else {
          reject(ValidationRejection("Message is not secure", None)).toDirective[Tuple1[BaseMessage]]
        }
      })
    }
  }

  def extractWithAgent(): Directive[Tuple2[BaseMessage, Option[AgentPointer]]] = {
    extractMessage() flatMap { msg ⇒
      msg.userId match {
        case Some(id) ⇒
          tprovide((msg, Utility.getAgent(id)))
        case None ⇒ reject(ValidationRejection("Could not get agent", None))
      }
    }
  }
}