package fb

import plenty.agent.model.Agent
import plenty.state.StateManager
import plenty.agent.AgentManager._
import plenty.network.{ActionIdentifiers, Network}
import plenty.state.model.{Node, RejectedBid}


/**
  * Saving state, modifying state by agents
  */
//class FbSendInterfaceTests extends TestSuite {
//
//  StateManager.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface$) }
//  FbAgent.load()
//
//
//  val u1id = "1783146675033183"
//  val arianne = Node("1269212013184777")
//  val anton = getAgents.find(_.id == u1id).get
//  val fbAgent = FbAgent.pointer.getAgentInLastKnownState
//  val donation = StateManager.createEmptyDonation(anton)
//
//  val tests = this {
//      'bids {
//        'accept_own {
//          val bid = StateManager.createBid(donation, 1, anton)
//          FbAgent.pointer.set({
//            val s = fbAgent.state.copy(bids = fbAgent.state.bids + bid)
//            fbAgent.copy(state = s)
//          })
//
//          Network.notifyAllAgents(bid, ActionIdentifiers.ACCEPT_BID_ACTION, anton)
//          waitClearQueue()
//        }
//
//        'accept_other {
//          val bidSelf = StateManager.createBid(donation, 1, anton)
//          val bidOther = StateManager.createBid(donation, 1, arianne)
//          FbAgent.pointer.set({
//            val s = fbAgent.state.copy(bids = fbAgent.state.bids + bidSelf + bidOther)
//            fbAgent.copy(state = s)
//          })
//
//          Network.notifyAllAgents(bidOther, ActionIdentifiers.ACCEPT_BID_ACTION, anton)
//          waitClearQueue()
//        }
//
//        'reject {
//          val bid = StateManager.createBid(donation, 1, anton)
//          val r = RejectedBid("just for fun", bid)
//          Network.notifyAllAgents(r, ActionIdentifiers.REJECT_BID_ACTION, anton)
//          waitClearQueue()
//        }
//
//        'auction_close {
//          val bidSelf = StateManager.createBid(donation, 1, anton)
//          val bidOther = StateManager.createBid(donation, 2, arianne)
//          val t = StateManager.createTransaction(Set(), arianne, anton).copy(bid = Option(bidOther))
//          FbAgent.pointer.set({
//            val s = fbAgent.state.copy(bids = fbAgent.state.bids + bidSelf + bidOther)
//            fbAgent.copy(state = s)
//          })
//
//          Network.notifyAllAgents(t, ActionIdentifiers.APPROVE_SETTLE_BID_ACTION, anton)
//          waitClearQueue()
//        }
//      }
//  }
//
//  def getAgents: Iterable[Agent] = {
//    Network.getAgents.map(_.getAgentInLastKnownState)
//  }
//
//  def waitClearQueue() = {
//    println("waiting on message queue")
//    println(s"non-completes: ${Network.nonCompletes.mkString(" ")}")
//    while (Network.nonCompletes.nonEmpty) {
//      Thread.sleep(1000)
//      println(s"non-completes (loop): ${Network.nonCompletes.mkString(" ")}")
//    }
//  }
//}

