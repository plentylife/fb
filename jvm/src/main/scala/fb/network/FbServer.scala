package fb.network

import java.io.{FileInputStream, InputStream}
import java.security.{KeyStore, SecureRandom}
import java.util.concurrent.TimeUnit
import java.util.logging.Logger
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpHeader.ParsingResult
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import fb.{FbSettings, ReceiverFlow}
import plenty.network.Network
import sun.misc.{Signal, SignalHandler}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.io.{Source, StdIn}
import scala.language.postfixOps
import scala.util.Failure

object FbServer {
  private val logger = Logger.getLogger("FbServer")
  private var bindingFuture: Future[Http.ServerBinding] = _
  private implicit val system: ActorSystem = ActorSystem.apply("my-system")
  private implicit val materializer: ActorMaterializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  private implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  /** for making http requests */
  private val httpClient = Http(system)
  def startAndWait(): Unit = {
    start()
    println(s"Server online\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    stop(() ⇒ ())
  }
  def start(): SignalHandler = {
    val webviewHeaders: Map[String, HttpHeader] = Seq("facebook.com", "messenger.com") map {
      host ⇒ host → HttpHeader.parse("X-Frame-Options", s"ALLOW-FROM https://www.$host/")
    } collect {
      case (host, h: ParsingResult.Ok) ⇒ host → h.header
    } toMap

    val route: Route =
      pathPrefix("backend") {
        // two paths, either from the webview or the webhook
        WebviewReceiver.route ~ pathPrefix("webhook") {
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
        }
      } ~ path("privacy-policy") {
        getFromFile(FbSettings.privacyPolicyFile)
      } ~ pathPrefix("webview") {
        optionalHeaderValueByName("Referer") { optRef ⇒
          val hostHeaders = optRef map { ref ⇒
            logger.finest(s"referrer is $ref")
            webviewHeaders.filter(h ⇒ ref.contains(h._1)).values.toSeq
          } getOrElse {logger.finer("no referrer"); Seq()}
          respondWithHeaders(hostHeaders: _*) {
            pathPrefix("resources") {
              logger.finest("serving directory")
              getFromDirectory(FbSettings.webviewResourceDir)
            } ~ {
              logger.finest("serving index")
              val index = Source.fromFile(FbSettings.indexFile)
              val modIndex = index.mkString
                .replace("[VIEW_PATH]", FbSettings.webviewViewPath)
                .replace("[BACK_PATH]", FbSettings.webviewBackendPath)
                .replace("[APP_ID]", FbSettings.appId)
              complete(HttpEntity(modIndex).withContentType(ContentTypes.`text/html(UTF-8)`))
            }
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


    bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080, connectionContext = https)
    handleSigTerm
  }
  def handleSigTerm: SignalHandler = {
    val handler = new SignalHandler {
      override def handle(signal: Signal): Unit = {
        println("=== TERMINATING ===")
        stop(() ⇒ {
          Await.ready(Network.waitUntilQueueClear, Duration.Inf)
          println("Network is torn down")
        })
      }
    }
    Signal.handle(new Signal("TERM"), handler)
    Signal.handle(new Signal("INT"), handler)
  }
  def stop(beforeTerminate: () ⇒ Unit): Unit = {
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete { _ =>
      println("Server is torn down")
      beforeTerminate()
      println("Final termination")
      system.terminate() // and shutdown actors when done
      System.exit(0)
    }
  }

  /** makes a request and retunrs the bytestring */
  def makeRequest(req: HttpRequest): String = {
    println("making request")
    val f = httpClient.singleRequest(req) flatMap {
      resp ⇒
        val res: Future[String] = resp.entity.dataBytes.runFold(ByteString(""))(_ ++ _) map { str ⇒
          try {
            val decodedStr = str.decodeString(resp.entity.contentType.charsetOption.getOrElse(HttpCharsets.`UTF-8`)
              .value)
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