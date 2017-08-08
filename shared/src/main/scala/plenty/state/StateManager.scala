package plenty.state

import java.io._
import java.nio.ByteBuffer
import java.security.SecureRandom
import java.util.Date

import plenty.agent.model.Agent
import plenty.state.model.{Coin, Donation, History, Node, State}
import boopickle.Default._

/**
  * The entry point into state management.
  *
  */
object StateManager {
  private val random = new SecureRandom()

  private def idGenerator(time: Long, additionalPiece: String = ""): String = {
    Seq(random.nextInt(Int.MaxValue), time).mkString("-")
  }

  def createDonation(title: String, description: String, by: Node) = {
    val now = new Date().getTime
    val id = idGenerator(now, title + by.id)
    Donation(id = id, title = title, description = description, by, now)
  }

  def updateHistory(oldState: State, newHistory: History): State = oldState.copy(history = newHistory)

  def save(agent: Agent) = {
    val archiveFilename = s"./data-stores/archive/${agent.id}-${new Date().getTime}.plenty"
    val currentFilename = s"./data-stores/current/${agent.id}.plenty"

    val archive = new BufferedOutputStream(new FileOutputStream(archiveFilename))
    val current = new BufferedOutputStream(new FileOutputStream(currentFilename))


    val pickle = Pickle.intoBytes(agent.state)

    while (pickle.remaining() > 0) {
      val next = pickle.get()
      archive.write(next)
      current.write(next)
    }

    archive.close() // You may end up with 0 bytes file if not calling close.
    current.close()
  }

  def load(agentId: String): Agent = {
    val filename = s"./data-stores/current/${agentId}.plenty"
    val reader = new BufferedInputStream(new FileInputStream(filename))

    var bytes = Stream[Byte]()
    while (reader.available() > 0) {
      bytes = bytes :+ reader.read().toByte
    }
    val unpickledState = Unpickle[State].fromBytes(ByteBuffer.wrap(bytes.toArray))

    Agent(id = agentId, state = unpickledState)
  }
}


