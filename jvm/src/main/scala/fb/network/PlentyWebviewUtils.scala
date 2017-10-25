package fb.network

import java.util.logging.Logger

import fb.UserInfo
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import plenty.agent.{Accounting, AgentPointer}
import plenty.network.{BidAction, Network}
import plenty.state.StateCodecs._
import plenty.state.StateManager
import plenty.state.model.Bid

import scala.language.postfixOps

object PlentyWebviewUtils {
  implicit val amountDecoder: Decoder[Amount] = deriveDecoder[Amount]

  def accountStatus(agent: AgentPointer): AccountStatus = {
    val balance = Accounting.getSelfBalance(agent.agentInLastState)
    val timeUntilDem = Accounting.timeUntilNextDemurage(agent.agentInLastState)
    AccountStatus(balance, timeUntilDem)
  }

  def highestCurrentBid(ap: AgentPointer, donationId: String): Option[Bid] = {
    val relBids = StateManager.getRelatedBids(ap.state, donationId)
    if (relBids.isEmpty) {
      None
    } else {
      Option(relBids maxBy (_.amount))
    }
  }

  def attachInfo(bid: Bid) = BidWithInfo(bid, UserInfo.get(bid.by.id))
  private val logger = Logger.getLogger("PlentyWebviewUtils")

  implicit val encoderAccountStatus: Encoder[AccountStatus] = deriveEncoder[AccountStatus]
  implicit val encoderBidInfo: Encoder[BidWithInfo] = deriveEncoder[BidWithInfo]
  implicit val encoderUserInfo: Encoder[UserInfo] = deriveEncoder[UserInfo]
  def bid(ap: AgentPointer, donationId: String, bidAmount: Int): Option[String] = {
    StateManager.getDonation(donationId)(ap.state) match {
      case None ⇒ Option("No such donation")
      case Some(d) ⇒
        val bid = StateManager.createBid(d, bidAmount, ap.node)
        logger.finer(s"Created bid $bid")
        Network.notifyAllAgents(bid, BidAction, from = ap.node)
        None
    }
  }
}

case class AccountStatus(balance: Int, timeUntilNextDemurrage: Long)

case class BidWithInfo(bid: Bid, info: UserInfo)

case class Amount(amount: Int)