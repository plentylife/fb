package plenty.network.communication

import plenty.agent.AgentManager._
import plenty.agent.model.Agent
import plenty.state.model.{Bid, Coin, Donation, Node}

/**
  * Created by anton on 8/8/17.
  */
private[communication] object Receiver {

  def receive(incomingMessage: Message[_])(implicit toAgent: Agent): Agent = incomingMessage match {
    case m if (m.payloadId.typeOfMsg == DonateAction.typeOfMsg) ||
      (m.payloadId.typeOfMsg == RelayIdentifiers.DONATION_RELAY.typeOfMsg) =>
      registerDonation(m.asInstanceOf[Message[Donation]].payload, toAgent)

    case m if (m.payloadId.typeOfMsg == BidAction.typeOfMsg) ||
      (m.payloadId.typeOfMsg == RelayIdentifiers.BID_RELAY.typeOfMsg) =>
      registerBid(m.asInstanceOf[Message[Bid]].payload, toAgent)

    case m if m.payloadId.typeOfMsg == ActionIdentifiers.BID_TAKE_ACTION =>
      registerTakenBid(m.asInstanceOf[Message[Bid]].payload, toAgent)

    case m if m.payloadId == ActionIdentifiers.COINS_MINTED =>
      registerCoins(m.asInstanceOf[Message[Set[Coin]]].payload, toAgent)

    case m if m.payloadId == ActionIdentifiers.SETTLE_BID_ACTION =>
      null

    case m if m.payloadId == ActionIdentifiers.REGISTER_NODE =>
      registerNode(m.payload.asInstanceOf[Node], toAgent)
  }
}
