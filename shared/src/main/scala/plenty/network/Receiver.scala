package plenty.network

import plenty.agent.AgentManager._
import plenty.agent.StateLogic
import plenty.agent.model.Agent
import plenty.state.model.{Bid, Coin, Donation, Node}

/**
  * Created by anton on 8/8/17.
  */
object Receiver {

  def receive(incomingMessage: Message[_])(implicit toAgent: Agent): Agent = incomingMessage match {
    case m if (m.payloadId.typeOfMsg == DonateAction.typeOfMsg) ||
      (m.payloadId.typeOfMsg == RelayIdentifiers.DONATION_RELAY.typeOfMsg) =>
      registerDonation(m.asInstanceOf[Message[Donation]], toAgent)

    case m if m.payloadId == ActionIdentifiers.COINS_MINTED =>
      registerCoins(m.asInstanceOf[Message[Set[Coin]]].payload, toAgent)

    case m if m.payloadId == ActionIdentifiers.SETTLE_BID_ACTION =>
      null

    case m if m.payloadId == ActionIdentifiers.REGISTER_NODE =>
      registerNode(m.payload.asInstanceOf[Node], toAgent)

//      bids

    case m if (m.payloadId == BidAction) ||
      (m.payloadId == RelayIdentifiers.BID_RELAY) =>
      verifyBid(m.asInstanceOf[Message[Bid]], toAgent)

    case m if m.payloadId == ActionIdentifiers.ACCEPT_BID_ACTION => StateLogic.registerBid(m.payload.asInstanceOf[Bid])
    case m if m.payloadId == ActionIdentifiers.REJECT_BID_ACTION => null
    case m if m.payloadId.typeOfMsg == ActionIdentifiers.BID_TAKE_ACTION =>
      registerTakenBid(m.asInstanceOf[Message[Bid]].payload, toAgent)

  }
}
