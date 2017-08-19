package plenty.state

import java.io._
import java.nio.ByteBuffer
import java.security.{MessageDigest, SecureRandom}
import java.util.{Base64, Date}

import plenty.agent.model.Agent
import plenty.state.model._
import boopickle.Default._

/**
  * The entry point into state management.
  *
  */
object StateManager {
  private val random = new SecureRandom()
  private val hasher = MessageDigest.getInstance("SHA-512")

  /* Creating objects */

  private def idGenerator(time: Long, additionalPiece: String = ""): String = {
    val idStr = Seq(random.nextInt(Int.MaxValue), time).mkString("-") + additionalPiece
    val idBytes = idStr.toCharArray map {_.toByte}
    val hash = hasher.digest(idBytes)
    Base64.getUrlEncoder.encodeToString(hash).replaceAll("=", "")
  }

  def createDonation(title: String, description: String, attachments: Seq[String], by: Node) = {
    val now = new Date().getTime
    val id = idGenerator(now, title + by.id)
    Donation(id = id, title = title, description = description, attachments=attachments, by, now)
  }

  def createBid(donation: Donation, amount: Int, by: Node): Bid = {
    val now = new Date().getTime
    val id = idGenerator(now, amount + by.id)
    Bid(id = id, donation = donation, amount=amount, by=by, timestamp = now)
  }

  def createTransaction(coins: Set[Coin], from: Node, to: Node) = {
    val now = new Date().getTime
    val id = idGenerator(now, s"transaction$from$to")
    Transaction(id, now, coins, from, to)
  }

  /* Selecting objects */

  /** @return bids that have the donation (of the given bid) in common.
    *         takes into account bids and non-settled bids */
  def getRelatedBids(state: State, bid: Bid): Set[Bid] = (state.bids ++ state.nonSettledBids) filter {
    _.donation == bid.donation}

  def updateHistory(oldState: State, newHistory: History): State = oldState.copy(history = newHistory)

  /* disk IO */

  def save(agent: Agent) = {
    val archiveFilename = s"./data-stores/archive/${agent.id}-${new Date().getTime}.plenty"
    val currentFilename = s"./data-stores/current/${agent.id}.plenty"

    val archive = new BufferedOutputStream(new FileOutputStream(archiveFilename))
    val current = new BufferedOutputStream(new FileOutputStream(currentFilename))


    val pickle = Pickle.intoBytes(agent)

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
    loadFromFile(filename)
  }

  private def loadFromFile(filename: String): Agent = {
    val reader = new BufferedInputStream(new FileInputStream(filename))

    var bytes = Seq[Byte]()
    while (reader.available() > 0) {
      bytes = bytes :+ reader.read().toByte
    }
    Unpickle[Agent].fromBytes(ByteBuffer.wrap(bytes.toArray))
  }

  def loadAll(): Set[Agent] = {
    val currentDir = new File("./data-stores/current/")
    val allAgentFiles = currentDir.listFiles()
    val agents = allAgentFiles map {f => loadFromFile(f.getAbsolutePath)}
    val agentSet = agents.toSet
    // making sure there isn't something wrong with saving
    require(agentSet.size == agents.size, "Duplicate agent files in current directory")
    agentSet
  }
}


