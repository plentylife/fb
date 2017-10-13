package fb.network

import java.util.logging.Logger

import fb.network.WebviewMessage.castArray
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, HCursor, Json}
import plenty.state.model.DescriptionToken

private[network] case class BaseMessage(signedRequest: String, payload: Json) extends WebviewMessage[Json] {
  private implicit val m: BaseMessage = this

  def toTokens: optm[Iterable[DescriptionToken]] = castArray[DescriptionToken]
}

private[network] trait WebviewMessage[T] extends SecureMessage {
  type optm[pt] = Option[WebviewMessage[pt]]

  val payload: T

  implicit protected val decoder: Decoder[DescriptionToken] = deriveDecoder[DescriptionToken]
  implicit protected val decoderAr: Decoder[Array[DescriptionToken]] = (c: HCursor) => {
    c.as[Array[Json]].map(_ flatMap {_.as[DescriptionToken].toOption}): Decoder.Result[Array[DescriptionToken]]
  }

  private def copyPayload[NewP](p: NewP): WebviewMessage[NewP] = WebviewMessage.copyWithPayload(p, this)
}


private object WebviewMessage {
  private[this] val logger = Logger.getLogger("WebviewMessage")

  def castArray[T: Decoder](implicit msg: WebviewMessage[Json]): Option[WebviewMessage[Iterable[T]]] = {
    msg.payload.asArray flatMap { arr: Vector[Json] ⇒
      val arrRes = arr.flatMap { elem: Json ⇒ cast[T](elem) }
      if (arrRes.nonEmpty) {
        Option(msg.copyPayload(arrRes))
      } else None
    }
  }

  def cast[T: Decoder](what: Json): Option[T] = {
    what.as[T].fold(e ⇒ {
      logger.fine(s"Failed to cast webview message payload: $e")
      None
    }, res ⇒ Option(res))
  }

  def cast[T: Decoder](implicit msg: WebviewMessage[Json]): Option[WebviewMessage[T]] = {
    msg.payload.as[T].fold(e ⇒ {
      logger.fine(s"Failed to cast webview message payload: $e")
      None
    }, p ⇒ Option(copyWithPayload(p, msg)))
  }

  def copyWithPayload[T](p: T, m: WebviewMessage[_]): WebviewMessage[T] = new WebviewMessage[T] {
    override val signedRequest: String = m.signedRequest
    override val payload: T = p
  }
}
