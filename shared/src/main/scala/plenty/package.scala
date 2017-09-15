import java.util.concurrent.ForkJoinPool
import java.util.logging.Level

import scala.concurrent.ExecutionContext

package object plenty {
  val cores: Set[Int] = Set(Runtime.getRuntime.availableProcessors, 2)
  val pool = new ForkJoinPool(cores.max)
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(pool)

  println(s"Pool set to ${cores.max} threads")

  import java.util.logging.LogManager
  import java.util.logging.Logger

  val rootLogger: Logger = LogManager.getLogManager.getLogger("")
  rootLogger.setLevel(Level.FINER)
  for (h <- rootLogger.getHandlers) {
    h.setLevel(Level.FINER)
  }

  def daysToMillis(d: Int) = d * 24 * 60 * 60 * 1000L
}
