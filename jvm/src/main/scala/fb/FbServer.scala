package fb

import java.io.{FileInputStream, InputStream}
import java.security.{KeyStore, SecureRandom}
import java.util.concurrent.TimeUnit
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.stream.ActorMaterializer
import akka.util.ByteString

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.io.{Source, StdIn}
import scala.language.postfixOps
import scala.util.Failure

object FbServer {
  private var bindingFuture: Future[Http.ServerBinding] = null
  private implicit val system = ActorSystem("my-system")
  private implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  private implicit val executionContext = system.dispatcher
  /** for making http requests */
  private val httpClient = Http(system)

  def start() = {

    val route: Route =
      pathPrefix("backend") {
        get {
          parameterMap { params =>
            // fixme implement this properly
            println("challenge", params.getOrElse("hub.challenge", "no-challenge"))
            val challenge: String = params.getOrElse("hub.challenge", "no-challenge")
            complete(challenge)
          }
        } ~
          post {
            entity(as[String]) { webhookMsg =>
              Future(ReceiverFlow.receive(webhookMsg)).onComplete({
                case Failure(e) =>
                  println(s"ERROR in webhook ${e.getMessage}\n${e.getStackTrace.mkString("\n")}")
                case _ => null
              })
              complete(StatusCodes.OK)
            }
          }
      } ~ path("privacy-policy") {
        getFromFile(FbSettings.privacyPolicyFile)
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


    bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080, connectionContext = https)
  }

  def startAndWait() = {
    start()
    println(s"Server online\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    stop
  }

  def stop() = {
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

  /** makes a request and retunrs the bytestring */
  def makeRequest(req: HttpRequest): String = {
    println("making request")
    val f = httpClient.singleRequest(req) flatMap {
      resp ⇒
        println("decoding request")
        println(s"dataBytes ${resp.entity.dataBytes}")
        val res: Future[String] = resp.entity.dataBytes.runFold(ByteString(""))(_ ++ _) map { str ⇒
          try {
            println(s"bytestring $str")
            println(s"charset ${resp.entity.contentType.charsetOption.getOrElse(HttpCharsets.`UTF-8`).value}")
            val decodedStr = str.decodeString(resp.entity.contentType.charsetOption.getOrElse(HttpCharsets.`UTF-8`)
              .value)
            println(s"decoded string $decodedStr")
            decodedStr

          } catch {
            case e: Throwable ⇒
              println(s"ERROR while decoding request. ${e.getMessage}")
              e.printStackTrace()
              ""
          }
        }
        println("future of decoded string")
        res
    }
    Await.result(f, Duration(20, TimeUnit.SECONDS))
  }
}
