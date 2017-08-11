package fb

import scala.io.Source

/**
  * Created by anton on 8/10/17.
  */
object AccessTokens {
  val pageToken = Source.fromFile("./private/page-token.txt").mkString.trim
  val webhookVerification = Source.fromFile("./private/webhook-verification.txt").mkString.trim
}
