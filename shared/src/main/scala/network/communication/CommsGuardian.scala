package network.communication

import java.util.Date

import agent.model.Agent
import state.model.{Bid, Node, Transaction, TransactionProbe}

/**
  * All communication requests are channeled through this module
  */
object CommsGuardian {

  /**
    * Sends out a message to all connected nodes
    * */
  def beacon(msgProto: MessagePrototype[_]) = {

  }

  def transactionProbe(probe: TransactionProbe, from: Agent) = {
    val msgProto = new MessagePrototype[TransactionProbe] {
      override def apply(to: Agent): Message[TransactionProbe] = {
        Message.createMessage(from, to, TransactionProbeAction, probe)
      }
    }

    beacon(msgProto)
  }

  def transact(transaction: Transaction, fromAgent: Agent) = {
    val msgProto = new MessagePrototype[Transaction] {
      override def apply(to: Agent): Message[Transaction] = {
        Message.createMessage(fromAgent, to, TransactionProbeAction, transaction)
      }
    }

    beacon(msgProto)
  }

  def acceptBidBeacon(bid: Bid, fromAgent: Agent) = {
    val msgProto = new MessagePrototype[Bid] {
      override def apply(to: Agent): Message[Bid] = {
        Message.createMessage(fromAgent, to, TransactionProbeAction, bid)
      }
    }

    beacon(msgProto: MessagePrototype[Bid])
  }
}
