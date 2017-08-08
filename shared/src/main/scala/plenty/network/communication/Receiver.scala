package plenty.network.communication

import plenty.agent.AgentManager._
import plenty.agent.model.Agent
import plenty.state.model.{Bid, Donation}

/**
  * Created by anton on 8/8/17.
  */
private[communication] object Receiver {

  def receive(msg: Message[_])(implicit toAgent: Agent): Agent = msg match {
    case m if (m.payloadId.typeOfMsg == DonateAction.typeOfMsg) ||
      (m.payloadId.typeOfMsg == ActionIdentifiers.DONATION_RELAY.typeOfMsg) =>
      registerDonation(m.asInstanceOf[Message[Donation]].payload, toAgent)
    case m if m.payloadId.typeOfMsg == BidAction.typeOfMsg => registerBid(m.asInstanceOf[Message[Bid]])
    case m if m.payloadId.typeOfMsg == BidAcceptAction.typeOfMsg => acceptBid(m.asInstanceOf[Message[Bid]])
  }
}
