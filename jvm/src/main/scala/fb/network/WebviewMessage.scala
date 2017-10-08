package fb.network

import java.util.logging.Logger

import fb.network.WebviewMessage.cast
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, HCursor, Json}
import plenty.state.model.DescriptionToken

private[network] case class BaseMessage(signedRequest: String, payload: Json) extends WebviewMessage[Json] {
  private implicit val m: BaseMessage = this

  def toTokens: optm[Array[DescriptionToken]] = cast[Array[DescriptionToken]]
}

private[network] trait WebviewMessage[T] extends SecureMessage {
  type optm[pt] = Option[WebviewMessage[pt]]

  val payload: T

  implicit protected val decoder: Decoder[DescriptionToken] = deriveDecoder[DescriptionToken]
  implicit protected val decoderAr: Decoder[Array[DescriptionToken]] = (c: HCursor) => {
    c.as[Array[Json]].map(_ flatMap {_.as[DescriptionToken].toOption}): Decoder.Result[Array[DescriptionToken]]
  }
}


private object WebviewMessage {
  private[this] val logger = Logger.getLogger("WebviewMessage")

  def cast[T: Decoder](implicit msg: WebviewMessage[Json]): Option[WebviewMessage[T]] = {
    msg.payload.as[T].fold(e ⇒ {
      logger.fine(s"Failed to cast webview message payload: $e")
      None
    }, p ⇒ Option(copyWithPayload(p, msg)))
  }

  private def copyWithPayload[T](p: T, m: WebviewMessage[_]): WebviewMessage[T] = new WebviewMessage[T] {
    override val signedRequest: String = m.signedRequest
    override val payload: T = p
  }
}
