import java.util.concurrent.ForkJoinPool

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

package object plenty {
  val cores: Int = Runtime.getRuntime.availableProcessors
  val pool = new ForkJoinPool(2)
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(pool)

  println(s"PROCESSING CORES $cores")
}
