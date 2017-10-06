package fb.network

import java.util.logging.Logger

import fb.donation.{DonationResponses, ExternalDonationUtils}
import fb.{FbAgent, FbState, Responses, UserInfo}
import plenty.network._
import plenty.state.model.{Bid, RejectedBid}


/** for interacting with Plenty by intercepting messages in the network */
object FbSendReceiveInterface extends SendReceiveInterface {
  private val logger = Logger.getLogger("Network [fb]")

  override def send(msg: Message[_]): Unit = {
    implicit val impMsg = msg
    logger.fine(s"$msg")

    // fixme. for now just passing on all traffic

    msg match {
      case m if m.payloadId == ActionIdentifiers.REGISTER_NODE =>
//        if (filterFbOnly(msg))
          Network.receive(msg)

      case m if m.payloadId == BidAction =>
//        passOnFbOnly
        Network.receive(msg)

      case m if m.payloadId == ActionIdentifiers.ACCEPT_BID_ACTION =>
        if (filterFbOnly(msg)) {
          val bid = msg.payload.asInstanceOf[Bid]
          if (bid.donation.by == m.from) {
            println("ACCEPT_BID_ACTION")
            Responses.bidEntered(bid)
          }
        }
        Network.receive(msg)

      case m if m.payloadId == ActionIdentifiers.REJECT_BID_ACTION =>
        if (filterFbOnly(msg)) {
          val rejection = msg.payload.asInstanceOf[RejectedBid]
          val bid = rejection.bid
          println(s"REJECT_BID reason ${rejection.reason} by ${msg.from}")
          if (bid.donation.by == m.from) {
            FbState.trackBid(Network.getAgents.find(_.id == bid.by.id).get, bid.donation)
            val uiBidder = UserInfo.get(rejection.bid.by.id)
            Responses.bidRejected(rejection, uiBidder)
          }
        }
        Network.receive(msg)

      case m if m.payloadId == ActionIdentifiers.APPROVE_SETTLE_BID_ACTION =>
        if (filterFbOnly(msg)) {
          val transaction = ActionIdentifiers.APPROVE_SETTLE_BID_ACTION.cast(msg.payload)
          if (transaction.to == msg.from) {
            // checking that only people who have bid on the item get the message
            DonationResponses.donationSettled(transaction)
            ExternalDonationUtils.markPostAsSettled(transaction.bid)
          }
        }
        Network.receive(msg)

      // fixme add ondenysettlebid

      case _ => Network.receive(msg)
    }
  }

  private def passOnFbOnly(implicit msg: Message[_]) = {
    if (filterFbOnly(msg)) {
      Network.receive(msg)
    }
  }

  private def filterFbOnly(msg: Message[_]): Boolean = {
    msg.to == FbAgent.node
  }
}