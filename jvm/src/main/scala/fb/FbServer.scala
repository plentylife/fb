package fb

import java.io.{FileInputStream, InputStream}
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.stream.ActorMaterializer

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{Source, StdIn}
import scala.util.Failure

object FbServer {
  def start() {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val route =
      pathEndOrSingleSlash {
        // for verification
        get {
          parameterMap { params =>
            // fixme implement this properly
            val challenge: String = params.getOrElse("hub.challenge", "no-challenge")
            complete(challenge)
          }
        } ~
          post {
            entity(as[String]) {webhookMsg =>
              Future(ReceiverFlow.receive(webhookMsg)).onComplete({
                case Failure(e) =>
                  println(s"ERROR in webhook ${e.getMessage}\n${e.getStackTrace.mkString("\n")}")
                case _ => null
              })
              complete(StatusCodes.OK)
            }
          }

      }

    val password = Source.fromFile("private/pass.txt").mkString.trim.toCharArray
    val ks: KeyStore = KeyStore.getInstance("jks")
    val keystore: InputStream = new FileInputStream("private/keystore.jks")

    require(keystore != null, "Keystore required!")
    ks.load(keystore, password)

    val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
    keyManagerFactory.init(ks, password)

    val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
    tmf.init(ks)

    val sslContext: SSLContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
    val https: HttpsConnectionContext = ConnectionContext.https(sslContext)


    val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080, connectionContext = https)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
