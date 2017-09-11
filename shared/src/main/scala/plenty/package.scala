import java.util.concurrent.ForkJoinPool

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

import plenty.agent.model.Agent
import plenty.state.model._
import io.circe._, io.circe.generic.semiauto._

package object plenty {
  val cores: Int = Runtime.getRuntime.availableProcessors
  val pool = new ForkJoinPool(2)
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(pool)

  println(s"PROCESSING CORES $cores")

  //
//  implicit val nodeEncoder = deriveEncoder[Node]
//  implicit val nodeDecoder = deriveDecoder[Node]
//
//  implicit val stateEncoder = deriveEncoder[State]
//  implicit val stateDecoder = deriveDecoder[State]
//
//  implicit val agentDecoder: Decoder[Agent] = Decoder.forProduct2("node", "state")(Agent.apply)
//  implicit val agentEncoder: Encoder[Agent] = Encoder.forProduct2("node", "state")(u ⇒
//    (u.node, u.state)
//  )


  def daysToMillis(d: Int) = d * 24 * 60 * 60 * 1000L
}
