package fb

import scala.io.Source

/**
  * Created by anton on 8/10/17.
  */
object FbSettings {
  var prod = Source.fromFile("./private/mode.txt").mkString.trim == "prod"
  lazy val fileMod = if (prod) ".prod" else ""

  lazy val s = Source.fromFile(s"./private/settings$fileMod.txt").getLines().map(_.trim).toSeq

  lazy val pageToken = s(1)
  lazy val pageId = s(3)
  lazy val appId = s(5)
  lazy val webhookVerification = Source.fromFile("./private/webhook-verification.txt").mkString.trim
  lazy val uri = s(7)
  lazy val webviewFolderPath = s(9)
  lazy val indexFile = webviewFolderPath + "index.html"

  println(s"Production mode on: $prod")
}
