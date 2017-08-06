package network.communication

import agent.model.Agent
import state.model.{Bid, Transaction}

/**
  * All communication requests are channeled through this module
  */
object CommsGuardian {

  /**
    * Sends out a message to all connected nodes
    * */
  def beacon(msgProto: MessagePrototype[_]) = {

  }

  def acceptBidBeacon(bid: Bid, fromAgent: Agent) = {
    val msgProto: MessagePrototype[Bid] = new MessagePrototype[Bid] {
      override def apply(toAgent: Agent): Message[Bid] = new Message[Bid] {
        override val to = toAgent
        override val from = fromAgent
        override val payloadId: PayloadIdentifier[Bid] = BidAcceptAction
        override val payload: Bid = bid
      }
    }

    beacon(msgProto: MessagePrototype[Bid])
  }

  def transact(transaction: Transaction, fromAgent: Agent) = {
    val msgProto = new MessagePrototype[Transaction] {
      override def apply(msgTo: Agent): Message[Transaction] = new Message[Transaction] {
        override val payloadId: PayloadIdentifier[Transaction] = TransactionAction
        override val payload: Transaction = transaction
        override val from: Agent = fromAgent
        override val to: Agent = msgTo
      }
    }

    beacon(msgProto)
  }
}
