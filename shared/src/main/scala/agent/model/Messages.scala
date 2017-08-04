package agent.model

import state.model.{Donation, Node}

/**
  * Various messaging formats
  */
private[agent] abstract class Message[P] {
  val from: Node
  val to: Node
  val payloadId: PayloadIdentifier[P]
  val payload: P
}

private[agent] trait PayloadIdentifier[P] {
  val typeOfMsg: String
}


object DonateAction extends PayloadIdentifier[Donation] {
  override val typeOfMsg: String = "DONATE_ACTION"
}

case class DonateMessage(from: Node, to: Node, payload: Donation) extends Message[Donation] {
  val payloadId: PayloadIdentifier[Donation] = DonateAction
}
