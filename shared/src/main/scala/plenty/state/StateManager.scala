package plenty.state

import java.io._
import java.security.{MessageDigest, SecureRandom}
import java.util.{Base64, Date}

import plenty.agent.model.Agent
import plenty.state.model._
import prickle.Pickler

/**
  * The entry point into state management.
  *
  */
object StateManager {
  private val random = new SecureRandom()
  private val hasher = MessageDigest.getInstance("SHA-512")

  /* Creating objects */

  private def idGenerator(time: Long, additionalPiece: String = ""): String = {
    val idStr = Seq(random.nextInt(Int.MaxValue), time).mkString("") + additionalPiece
    val idBytes = idStr.toCharArray map {_.toByte}
    val hash = hasher.digest(idBytes)
    Base64.getUrlEncoder.encodeToString(hash).replaceAll("=", "")
  }

  /** @return donation with the id, by, and timestamp filled only */
  def createEmptyDonation(by: Node): Donation = {
    val now = new Date().getTime
    val id = idGenerator(now, by.id)
    Donation(id = id, by = by, timestamp = now)
  }

  def createBid(donation: Donation, amount: Int, by: Node): Bid = {
    val now = new Date().getTime
    val id = idGenerator(now, amount + by.id)
    Bid(id = id, donation = donation, amount = amount, by = by, timestamp = now)
  }

  def createTransaction(coins: Set[Coin], from: Node, to: Node): Transaction = {
    val now = new Date().getTime
    val id = idGenerator(now, s"$from$to$now")
    BaseTransaction(id, now, coins, from, to)
  }

  def asDemurage(t: Transaction): DemurageTransaction = {
    DemurageTransaction(t.id, t.timestamp, t.coins, from = t.from, to = t.to)
  }

  def transformTransaction(t: Transaction, bid: Bid): BidTransaction = {
    BidTransaction(t.id, t.timestamp, t.coins, from = t.from, to = t.to, bid)
  }


  /* Selecting objects */

  /** @return bids that have the donation (of the given bid) in common.
    *         takes into account bids and non-settled bids */
  def getRelatedBids(state: State, bid: Bid): Set[Bid] = (state.bids ++ state.nonSettledBids) filter {
    _.donation == bid.donation
  }

  def updateChains(state: State, newChains: Chains): State = state.copy(chains = newChains)

  /* disk IO */

  implicit val agentPickler: Pickler[Agent] = Pickler.materializePickler[Agent]

  def save(agent: Agent): Unit = {
    val archiveFilename = s"./data-stores/archive/${agent.id}-${new Date().getTime}.plenty"
    val currentFilename = s"./data-stores/current/${agent.id}.plenty"

    //    val archive = new BufferedOutputStream(new FileOutputStream(archiveFilename))
    val current = new BufferedOutputStream(new FileOutputStream(currentFilename))

    val pickle = prickle.Pickle.intoString(agent)

    // fixme. archive should be functional as well
    //    new PrintWriter(archive).write(pickle)
    val w = new PrintWriter(current)
    w.write(pickle)
    w.flush()
    w.close()

    //    archive.close() // You may end up with 0 bytes file if not calling close.
    current.close()
  }

  def load(agentId: String): Agent = {
    val filename = s"./data-stores/current/$agentId.plenty"
    loadFromFile(filename)
  }

  private def loadFromFile(filename: String): Agent = {
    val reader = new BufferedInputStream(new FileInputStream(filename))

    var bytes = Seq[Byte]()
    while (reader.available() > 0) {
      bytes = bytes :+ reader.read().toByte
    }
    // fixme terrible
    prickle.Unpickle[Agent].fromString(bytes.map {_.toChar}.mkString).getOrElse(Agent(Node("fake"), State()))
  }

  def loadAll(): Set[Agent] = {
    val currentDir = new File("./data-stores/current/")
    val allAgentFiles = currentDir.listFiles()
    val agents = allAgentFiles map { f => loadFromFile(f.getAbsolutePath) }
    val agentSet = agents.toSet
    // making sure there isn't something wrong with saving
    require(agentSet.size == agents.length, "Duplicate agent files in current directory")
    agentSet
  }
}


