package fb.network

import fb.{UserInfo, Utility}
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import plenty.agent.{Accounting, AgentPointer}
import plenty.state.StateCodecs._
import plenty.state.StateManager
import plenty.state.model.Bid

import scala.language.postfixOps

object PlentyWebviewUtils {
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

  def bid(ap: AgentPointer, donationId: String, bidAmount: String): Option[String] = {
    StateManager.getDonation(donationId)(ap.state) match {
      case None ⇒ Option("No such donation")
      case Some(d) ⇒
      Utility.processTextAsBid(bidAmount, d, ap) match {
        case true ⇒
          None
        case false ⇒
          Option("This is not a whole number")
      }
    }
  }

  implicit val encoderAccountStatus: Encoder[AccountStatus] = deriveEncoder[AccountStatus]
  implicit val encoderBidInfo: Encoder[BidWithInfo] = deriveEncoder[BidWithInfo]
  implicit val encoderUserInfo: Encoder[UserInfo] = deriveEncoder[UserInfo]

}

case class AccountStatus(balance: Int, timeUntilNextDemurrage: Long)

case class BidWithInfo(bid: Bid, info: UserInfo)