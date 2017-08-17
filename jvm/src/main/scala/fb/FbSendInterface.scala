package fb

import plenty.agent.StateLogic
import plenty.network.{ActionIdentifiers, Message, Network, SendInterface}
import plenty.state.model.{Bid, RejectedBid}


/** for interacting with Plenty by intercepting messages in the network */
object FbSendInterface extends SendInterface {
  override def send(msg: Message[_]): Unit = {
    println(s"FB NET: $msg")
    msg match {
      case m if m.payloadId == ActionIdentifiers.REGISTER_NODE =>
        if (filterFbOnly(msg)) Network.receive(msg)

      case m if m.payloadId == ActionIdentifiers.ACCEPT_BID_ACTION =>
        if (filterFbOnly(msg)) {
          val uiFrom = UserInfo.get(msg.from.id)
          val bid = msg.payload.asInstanceOf[Bid]
          Responses.bidEntered(bid, FbState.popBid(bid), uiFrom)
          Network.receive(msg)
        }
      case m if m.payloadId == ActionIdentifiers.REJECT_BID_ACTION =>
        if (filterFbOnly(msg)) {
          val uiFrom = UserInfo.get(msg.from.id)
          val rejection = msg.payload.asInstanceOf[RejectedBid]
          Responses.bidRejected(rejection, FbState.popBid(rejection.bid), uiFrom)
          Network.receive(msg)
        }

      case _ => Network.receive(msg)
    }
  }

  private def filterFbOnly(msg: Message[_]): Boolean = {
    msg.to == FbAgent.node
  }
}