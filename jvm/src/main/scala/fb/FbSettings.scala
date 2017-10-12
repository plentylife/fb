package fb

import scala.io.Source

/**
  * Created by anton on 8/10/17.
  */
object FbSettings {
  lazy val fileMod: String = if (prod) ".prod" else ""
  lazy val s: Seq[String] = Source.fromFile(s"./private/settings$fileMod.txt").getLines().map(_.trim).toSeq
  lazy val webhookVerification: String = Source.fromFile("./private/webhook-verification.txt").mkString.trim

  lazy val pageToken = s(1)
  lazy val pageId = s(3)
  lazy val appId = s(5)
  lazy val appSecret = s(15)
  lazy val indexFile: String = webviewFolderPath + "index.html"
  lazy val uri = s(7)
  lazy val webviewFolderPath = s(9)
  lazy val webviewResourceDir: String = webviewFolderPath + "/resources"
  lazy val webviewViewPath: String = if (prod) "/webview" else "/test/webview"
  lazy val webviewBackendPath: String = if (prod) "/backend/webview" else "/test/backend/webview"
  var prod: Boolean = Source.fromFile("./private/mode.txt").mkString.trim == "prod"
  lazy val googleShortnerApiKey = s(11)
  lazy val privacyPolicyFile = s(13)

  println(s"Production mode on: $prod")
}
