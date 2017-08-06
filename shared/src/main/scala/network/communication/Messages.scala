package network.communication

import java.util.Date

import agent.model.Agent
import state.model.{Bid, Donation, Transaction}

/**
  * Various messaging formats
  */
abstract class Message[P] {
  val from: Agent
  val to: Agent
  val payloadId: PayloadIdentifier[P]
  val payload: P

  val timestamp = new Date().getTime
}

trait PayloadIdentifier[P] {
  val typeOfMsg: String
}

trait MessagePrototype[P] extends Function1[Agent, Message[P]]


object DonateAction extends PayloadIdentifier[Donation] {
  override val typeOfMsg: String = "DONATE_ACTION"
}
object BidAction extends PayloadIdentifier[Bid] {
  override val typeOfMsg: String = "BID_ACTION"
}
object BidAcceptAction extends PayloadIdentifier[Bid] {
  override val typeOfMsg: String = "BID_ACCEPT_ACTION"
}
object TransactionAction extends PayloadIdentifier[Transaction] {
  override val typeOfMsg: String = "TRANSACTION_ACTION"
}


case class DonateMessage(from: Agent, to: Agent, payload: Donation) extends Message[Donation] {
  val payloadId: PayloadIdentifier[Donation] = DonateAction
}
