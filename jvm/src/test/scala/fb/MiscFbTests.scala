package fb

import org.scalatest.FreeSpec
import plenty.state.StateManager
import plenty.state.model.Node

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  */
class MiscFbTests extends FreeSpec {

  "Google shortner service" - {
    val f = Utility.getShortLink("plenty.life")
    val link = Await.result(f, Duration.Inf)
    println(link)

    "should produce a valid link" in {
      assert(link.nonEmpty)
    }
  }
}
