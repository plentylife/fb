import java.util.concurrent.ForkJoinPool

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

import plenty.agent.model.Agent
import plenty.state.model._
import prickle.{CompositePickler, Pickler}

package object plenty {
  val cores: Int = Runtime.getRuntime.availableProcessors
  val pool = new ForkJoinPool(2)
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(pool)

  println(s"PROCESSING CORES $cores")

  implicit val transPickler = CompositePickler[Transaction].concreteType[BidTransaction]
    .concreteType[BaseTransaction].concreteType[DemurageTransaction]
  implicit val chainsPickler: Pickler[Chains] = Pickler.materializePickler[Chains]
  implicit val statePickler: Pickler[State] = Pickler.materializePickler[State]


  def daysToMillis(d: Int) = d * 24 * 60 * 60 * 1000L
}
