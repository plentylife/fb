package agent

import agent.model._
import network.communication._
import state.StateGuardian
import state.model.{Bid, Donation, State}

/**
  * Facilitates interaction between two [[agent.model.Agent]]
  */
private object StateLogic {
  def updateState(message: Message[_])(implicit agent: Agent): Agent =
  message match {
    case m if m.payloadId.typeOfMsg == DonateAction.typeOfMsg => donate(m.asInstanceOf[DonateMessage])
    case m if m.payloadId.typeOfMsg == BidAction.typeOfMsg => bid(m.asInstanceOf[Message[Bid]])
    case m if m.payloadId.typeOfMsg == BidAcceptAction.typeOfMsg => bidAcceptance(m.asInstanceOf[Message[Bid]])
  }

  private def bidAcceptance(message: Message[Bid])(implicit agent: Agent): Agent = {
    //fixme SECURITY
    val safePayload = message.payload
    val stateUpdated = agent.state.copy(
      donations = agent.state.donations.filterNot(_.id == message.payload.donation.id),
      bids = agent.state.bids.filterNot(_.id == message.payload.id),
      nonSettledBids = agent.state.nonSettledBids + message.payload
    )
    val historyUpdated = agent.history.copy(
      donations = agent.history.donations + message.payload.donation,
      bids = agent.history.bids + message.payload
    )
    val agentUpdated = agent.copy(state = stateUpdated, history = historyUpdated)

    StateGuardian.save(agentUpdated)
    agentUpdated
  }

  private def bid(message: Message[Bid])(implicit agent: Agent): Agent = {
    //fixme SECURITY
    val safePayload = message.payload
    val stateUpdated = agent.state.copy(
      bids = agent.state.bids + message.payload
    )
    val agentUpd = agent.copy(state = stateUpdated)

    StateGuardian.save(agentUpd)
    ActionLogic.receiveBid(agentUpd)

    agentUpd
  }

  private def donate(message: DonateMessage)(implicit agent: Agent): Agent = {
    //fixme SECURITY
    val safePayload = message.payload

    val stateUpdated = agent.state.copy(
      donations = agent.state.donations + message.payload
    )
    val agentUpd = agent.copy(state = stateUpdated)

    StateGuardian.save(agentUpd)

    agentUpd
  }


}
