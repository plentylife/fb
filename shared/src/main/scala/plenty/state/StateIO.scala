package plenty.state

import java.io.{BufferedOutputStream, File, FileOutputStream, PrintWriter}

import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import plenty.agent.model.Agent
import plenty.state.model._

/** Loading and saving of [[plenty.agent.model.Agent]] and [[plenty.state.model.State]] */
trait StateIO[LoadType] {
  implicit val decoder: Decoder[LoadType]
  /* disk IO */

  def save(agent: Agent, subfolder: String = "current/"): Unit = {
    val currentFilename = s"./data-stores/$subfolder${agent.id}.plenty"

    val current = new BufferedOutputStream(new FileOutputStream(currentFilename))

    val pickle = agent.asJson.noSpaces

    val w = new PrintWriter(current)
    w.write(pickle)
    w.flush()
    w.close()

    //    archive.close() // You may end up with 0 bytes file if not calling close.
    current.close()
  }

  def load(agentId: String, subFolder: String = "current/"):
  Option[LoadType] = {
    val filename = s"./data-stores/$subFolder$agentId.plenty"
    if (new File(filename).exists())
      loadFromFile(filename)
    else None
  }

  private def loadFromFile(filename: String): Option[LoadType] = {
    val source = scala.io.Source.fromFile(filename).mkString
    val decoderRes = decode[LoadType](source)
    decoderRes.toOption
  }

  def loadAll(subFolder: String = "current/"): Set[LoadType] = {
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

import io.circe._
import io.circe.generic.semiauto._

object StateIO extends StateIO[Agent] {
  override implicit val decoder: Decoder[Agent] = deriveDecoder[Agent]
}

object StateCodecs {

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

