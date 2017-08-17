package fb

import plenty.agent.StateLogic
import plenty.network._
import plenty.state.model.{Bid, RejectedBid}


/** for interacting with Plenty by intercepting messages in the network */
object FbSendInterface extends SendInterface {
  override def send(msg: Message[_]): Unit = {
    implicit val impMsg = msg
    println(s"FB NET: $msg")

    msg match {
      case m if m.payloadId == ActionIdentifiers.REGISTER_NODE =>
        if (filterFbOnly(msg)) Network.receive(msg)

      case m if m.payloadId == BidAction => passOnFbOnly
      case m if m.payloadId == ActionIdentifiers.ACCEPT_BID_ACTION =>
        if (filterFbOnly(msg)) {
          val bid = msg.payload.asInstanceOf[Bid]
          val uiFrom = UserInfo.get(bid.by.id)
          Responses.bidEntered(bid)
          Network.receive(msg)
        }
      case m if m.payloadId == ActionIdentifiers.REJECT_BID_ACTION =>
        if (filterFbOnly(msg)) {
          val rejection = msg.payload.asInstanceOf[RejectedBid]
          val uiFrom = UserInfo.get(rejection.bid.by.id)
          Responses.bidRejected(rejection, uiFrom)
          Network.receive(msg)
        }

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