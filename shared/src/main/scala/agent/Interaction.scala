package agent

import agent.model.{DonateAction, DonateMessage, Message}
import state.model.{Donation, State}

/**
  * Facilitates interaction between two [[agent.model.Agent]]
  */
private object Interaction {
  def updateState(message: Message[_])(implicit agentState: State): State =
  message match {
    case m if m.payloadId.typeOfMsg == DonateAction.typeOfMsg => donate(m.asInstanceOf[DonateMessage])
  }

  private def donate(message: DonateMessage)(implicit agentState: State): State = {


    //fixme
    val safePayload = message.payload

    agentState.copy(
      donations = agentState.donations + message.payload
    )
  }
}
