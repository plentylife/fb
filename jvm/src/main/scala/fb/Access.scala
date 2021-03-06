package fb

import scala.io.Source

/**
  * Created by anton on 8/10/17.
  */
object Access {
  val pageToken = Source.fromFile("./private/page-token.txt").mkString.trim
  val pageId = Source.fromFile("./private/page-id.txt").mkString.trim
  val webhookVerification = Source.fromFile("./private/webhook-verification.txt").mkString.trim
  val uri = Source.fromFile("./private/uri.txt").mkString.trim
}
