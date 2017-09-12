import java.util.concurrent.ForkJoinPool

import scala.concurrent.ExecutionContext

package object plenty {
  val cores: Set[Int] = Set(Runtime.getRuntime.availableProcessors, 2)
  val pool = new ForkJoinPool(cores.max)
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(pool)

  println(s"Pool set to ${cores.max} threads")

  def daysToMillis(d: Int) = d * 24 * 60 * 60 * 1000L
}
