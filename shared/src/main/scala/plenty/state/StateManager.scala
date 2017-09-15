package plenty.state

import java.io._
import java.security.{MessageDigest, SecureRandom}
import java.util.{Base64, Date}

import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import plenty.agent.model.Agent
import plenty.state.model._

/**
  * The entry point into state management.
  *
  */
object StateManager {
  import Codecs._

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

  def save(agent: Agent): Unit = {
    val archiveFilename = s"./data-stores/archive/${agent.id}-${new Date().getTime}.plenty"
    val currentFilename = s"./data-stores/current/${agent.id}.plenty"

    //    val archive = new BufferedOutputStream(new FileOutputStream(archiveFilename))
    val current = new BufferedOutputStream(new FileOutputStream(currentFilename))

    val pickle = agent.asJson.noSpaces

    // fixme. archive should be functional as well
    //    new PrintWriter(archive).write(pickle)
    val w = new PrintWriter(current)
    w.write(pickle)
    w.flush()
    w.close()

    //    archive.close() // You may end up with 0 bytes file if not calling close.
    current.close()
  }

  def load(agentId: String, subFolder: String ="current/"): Option[Agent] = {
    val filename = s"./data-stores/$subFolder$agentId.plenty"
    if (new File(filename).exists())
      loadFromFile(filename)
    else None
  }

  private def loadFromFile(filename: String): Option[Agent] = {
    val source = scala.io.Source.fromFile(filename).mkString
    val decoderRes = decode[Agent](source)
    decoderRes.toOption
  }

  def loadAll(subFolder: String = "current/"): Set[Agent] = {
    val currentDir = new File(s"./data-stores/$subFolder")
    val allAgentFiles = currentDir.listFiles()
    val agents = allAgentFiles flatMap { f => loadFromFile(f.getAbsolutePath) }
    val agentSet = agents.toSet
    // making sure there isn't something wrong with saving
    require(agents.length == allAgentFiles.length, "Wrong file formatting. Could not load some agents")
    require(agentSet.size == agents.length, "Duplicate agent files in current directory")
    agentSet
  }
}

object Codecs {
  import io.circe._, io.circe.generic.semiauto._

  implicit val ttypeEnc: Encoder[TransactionType.Value] = Encoder.forProduct1("type")(u ⇒ u.toString)
  implicit val nodeEnc: Encoder[Node] = deriveEncoder[Node]
  implicit val coinEnc: Encoder[Coin] = deriveEncoder[Coin]
  implicit val donationEnc: Encoder[Donation] = deriveEncoder[Donation]
  implicit val bidEnc: Encoder[Bid] = deriveEncoder[Bid]
  implicit val transactionEncoder: Encoder[Transaction] {
    def apply(a: Transaction): Json
  } = new Encoder[Transaction] {
    override def apply(a: Transaction): Json = {
      val base = a match {
        case t: BidTransaction ⇒ deriveEncoder[BidTransaction].apply(t)
        case t: BaseTransaction ⇒ deriveEncoder[BaseTransaction].apply(t)
        case t: DemurageTransaction ⇒ deriveEncoder[DemurageTransaction].apply(t)
      }
      base.deepMerge(ttypeEnc.apply(a.transactionType))
    }
  }
  implicit val chainsEnc: Encoder[Chains] = deriveEncoder[Chains]


  implicit val ttypeDec: Decoder[TransactionType.Value] = Decoder.forProduct1("type")(TransactionType.withName)
  implicit val nodeDec: Decoder[Node] = deriveDecoder[Node]
  implicit val coinDec: Decoder[Coin] = deriveDecoder[Coin]
  implicit val donationDec: Decoder[Donation] = deriveDecoder[Donation]
  implicit val bidDec: Decoder[Bid] = deriveDecoder[Bid]
  implicit val chainsDec: Decoder[Chains] = deriveDecoder[Chains]
  implicit val bidtDec: Decoder[BidTransaction] = deriveDecoder[BidTransaction]
  implicit val basetDec: Decoder[BaseTransaction] = deriveDecoder[BaseTransaction]
  implicit val demtDec: Decoder[DemurageTransaction] = deriveDecoder[DemurageTransaction]
  implicit val transactionDec: Decoder[Transaction] = (c: HCursor) => c.as(ttypeDec).toOption.get match {
    case TransactionType.BID ⇒ c.as(bidtDec) map {_.asInstanceOf[Transaction]}
    case TransactionType.BASE ⇒ c.as(basetDec) map {_.asInstanceOf[Transaction]}
    case TransactionType.DEMURAGE ⇒ c.as(demtDec) map {_.asInstanceOf[Transaction]}
  }
}

