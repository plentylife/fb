package onserver

import java.util.Date

import fb.{FbAgent, FbSettings, UserInfo}
import org.scalatest.{FreeSpec, Matchers}
import plenty.MockSendReceiveInterface
import plenty.TestUtilities._
import plenty.agent.AgentManager.agentAsNode
import plenty.agent.model.Agent
import plenty.network.{ActionIdentifiers, BidAction, Message, Network}
import plenty.state.StateManager
import plenty.state.model.{Bid, RejectedBid}

import scala.language.postfixOps

class BiddingTests extends FreeSpec with Matchers {

  FbSettings.prod = true
  val as: Set[Agent] = StateManager.loadAll("onserver/")
  as filterNot (_.id == "facebook_agent") foreach { a ⇒
    val ui = UserInfo.get(a.id)
    println(ui.name, a.id)
  }

  "When bidding a user should not send more than one message per bid" in {

    val anton = as find {_.id == "767613720030082"} get
    val gyorgy = as find {_.id == "1495520050542318"} get
    val donation = anton.state.donations find {_.title.get.toLowerCase contains "baking"} get
    val bid = Bid("fake", donation, 6, gyorgy, timestamp = new Date().getTime)

    val rf = (a: Agent) ⇒ Network.registerAgent(a, MockSendReceiveInterface)
    val aps = as map rf

    // trying to recreate the bug
    val msgToFb = Message.createMessage(gyorgy, FbAgent.node, BidAction, bid)
    val msgToSelf = Message.createMessage(gyorgy, gyorgy, BidAction, bid)

    Network.send(msgToFb)
    waitClearQueue()
    Network.send(msgToSelf)
    waitClearQueue()

    val reasons = MockSendReceiveInterface.log.toSet.collect({
      case m: Message[RejectedBid] if m.payloadId == ActionIdentifiers.REJECT_BID_ACTION ⇒ m.payload.reason
    }: PartialFunction[Message[_], String])

    div("Reasons")
    println(s"Num reasons ${reasons.size}")
    reasons foreach println

    reasons shouldBe empty
  }

}