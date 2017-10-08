package fb.network

import java.util.logging.Logger

import akka.http.scaladsl.server.Directives._

private[network] object WebviewReceiver {
  val route = pathPrefix("webview") {
    logger.finer("webview route")
    pathPrefix("donation") {
      logger.finer("past donate clause")
      post {
        logger.finer("past post clause")
        WebviewSecurity.extractMessage() { msg ⇒

        }
      }
    }
  }
  private[this] val logger = Logger.getLogger("Webview Receiver")

}
