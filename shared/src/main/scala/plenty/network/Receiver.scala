package plenty.network

import plenty.agent.AgentManager._
import plenty.agent.{ActionLogic, AgentManager, StateLogic}
import plenty.agent.model.Agent
import plenty.state.model._

/**
  * Created by anton on 8/8/17.
  */
object Receiver {

  // fixme this can be rewritten to match directly on incomingMessage.payload
  def receive(incomingMessage: Message[_])(implicit toAgent: Agent): Agent = incomingMessage match {
    case m if m.payloadId == ActionIdentifiers.REGISTER_NODE =>
    registerNode(m.payload.asInstanceOf[Node], toAgent)

    case m if m.payloadId == ActionIdentifiers.COINS_MINTED =>
        // fixme. firuger out what to do with this message type
      //registerCoins(m.asInstanceOf[Message[Set[Coin]]].payload, toAgent)
      toAgent

    /* Transactions */

    case m if m.payloadId == ActionIdentifiers.SETTLE_BID_ACTION =>
    ActionLogic.verifyTransactionForBid(m.payload.asInstanceOf[BidTransaction], toAgent)
      toAgent

    case m if m.payloadId == ActionIdentifiers.APPROVE_SETTLE_BID_ACTION =>
      val t = m.payload.asInstanceOf[BidTransaction]
      onApproveSettleBid(t, toAgent, m)

    case m if m.payloadId == ActionIdentifiers.DENY_SETTLE_BID_ACTION =>
      val t = m.payload.asInstanceOf[RejectedTransaction[BidTransaction]].transaction
      onDenySettleBid(t, toAgent, m)

    /* Bids */

    case m if (m.payloadId == BidAction) ||
      (m.payloadId == RelayIdentifiers.BID_RELAY) =>
    verifyBid(m.asInstanceOf[Message[Bid]], toAgent)

    case m if m.payloadId == ActionIdentifiers.ACCEPT_BID_ACTION =>
//      println(s"agent ${toAgent.id} accepting bid ${m.payload}")
      StateLogic.registerBid(m.payload.asInstanceOf[Bid])

    case m if m.payloadId == ActionIdentifiers.REJECT_BID_ACTION =>
      /* nothing happens */
      toAgent

    case m if m.payloadId == ActionIdentifiers.BID_TAKE_ACTION =>
      registerTakenBid(m.asInstanceOf[Message[Bid]].payload, toAgent)

    case m if m.payloadId == ActionIdentifiers.RETRACT_BID_ACTION =>
      retractBid(m.asInstanceOf[Message[Bid]].payload, toAgent)

      /* Donations */

    case m if (m.payloadId.typeOfMsg == DonateAction.typeOfMsg) ||
      (m.payloadId.typeOfMsg == RelayIdentifiers.DONATION_RELAY.typeOfMsg) =>
      registerDonation(m.asInstanceOf[Message[Donation]], toAgent)

    case _ ⇒ properMatchingFunction(incomingMessage)
  }

  /** this is how the matching function should have been written from the beginning*/
  private def properMatchingFunction(incomingMessage: Message[_])(implicit toAgent: Agent): Agent = {
    val m = incomingMessage
    incomingMessage.payloadId match {
      case ActionIdentifiers.TRANSACTION ⇒
        val p = ActionIdentifiers.TRANSACTION.cast(m.payload)
        ActionLogic.verifyTransaction(p, toAgent)
        toAgent

      case ActionIdentifiers.ACCEPT_TRANSACTION ⇒
        val p = ActionIdentifiers.ACCEPT_TRANSACTION.cast(m.payload)
        AgentManager.onAcceptTransaction(p, toAgent, m)
      case ActionIdentifiers.REJECT_TRANSACTION ⇒
        toAgent
    }
  }
}
