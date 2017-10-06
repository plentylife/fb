package onserver

import java.util.Date
import java.util.logging.Logger

import fb.{FbAgent, FbSettings, UserInfo}
import org.scalatest.{FreeSpec, Matchers}
import plenty.MockSendReceiveInterface
import plenty.TestUtilities._
import plenty.agent.agentAsNode
import plenty.agent.logic.ActionLogic
import plenty.agent.model.Agent
import plenty.network._
import plenty.state.StateIO
import plenty.state.model._

import scala.language.postfixOps

class BiddingTests extends FreeSpec with Matchers {
  private val logger = Logger.getAnonymousLogger()

  FbSettings.prod = true
  val as: Set[Agent] = StateIO.loadAll("onserver/")
  as filterNot (_.id == "facebook_agent") foreach { a ⇒
    val ui = UserInfo.get(a.id)
    println(ui.name, a.id)
  }

  "When bidding a user should not send more than one message per bid" in {

    val anton = as find {_.id == "767613720030082"} get
    val gyorgy = as find {_.id == "1495520050542318"} get
    //    val donation = anton.state.donations find {_.title.get.toLowerCase contains "baking"} get
    val donation = ???
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
      case m: Message[_] if m.payloadId == ActionIdentifiers.REJECT_BID_ACTION ⇒
        ActionIdentifiers.REJECT_BID_ACTION.cast(m.payload).reason
    }: PartialFunction[Message[_], String])

    div("Reasons")
    println(s"Num reasons ${reasons.size}")
    reasons foreach println

    reasons shouldBe empty
  }

  "Verifying bid transactions should happen only once per bid transaction" in {
    import ActionIdentifiers._
    import com.softwaremill.quicklens._
    Network.clear

    var cs: Set[Coin] = MintPress.fillCoinSet(Set(), as.toList(0).node)
    cs = cs.grouped(10).toSeq zip as flatMap { case (c, a) ⇒
      c map {_.copy(belongsTo = a.node)}
    } toSet

    val bids: List[Bid] = List()
    val modBids = bids map {_.modify(_.timestamp).using(_ - 2 * 24 * 60 * 60 * 1000)}

    val modAs = as map {
      _.modify(_.state.bids).using { bs ⇒ modBids.toSet }
        .modify(_.state.coins).using(_ ⇒ cs)
    }
    val rf = (a: Agent) ⇒ Network.registerAgent(a, MockSendReceiveInterface)
    val aps = modAs map rf

    aps foreach { ap ⇒ ActionLogic.takeBids(ap.agentInLastState) }
    waitClearQueue()

    div("FbReceived")
    val countAsba = MockSendReceiveInterface.log filter {
      _.payloadId == ActionIdentifiers.APPROVE_SETTLE_BID_ACTION
    } groupBy { l ⇒ l.from.id + " -> " + l.to.id }
    val countSbaTo = MockSendReceiveInterface.log filter {
      _.payloadId == ActionIdentifiers.SETTLE_BID_ACTION
    } groupBy { l ⇒ l.to.id }
    val countBtaTo = MockSendReceiveInterface.log filter {
      _.payloadId == ActionIdentifiers.BID_TAKE_ACTION
    } groupBy { l ⇒ l.to.id }

    //    printGrouped(countSba)
    //    printGrouped(countAsba)


    val asbaAnton = countAsba("767613720030082 -> facebook_agent")
    val sbaAnton = countSbaTo("767613720030082")
    val btaSaladSpinner = countBtaTo("1665771990162012")
    logger.info(s"Total messages exchanged ${MockSendReceiveInterface.log.size}")
    logger.info(s"Messages from Anton to FB ${asbaAnton.size} of type ${APPROVE_SETTLE_BID_ACTION}")
    Thread.sleep(100)
    asbaAnton foreach {println}
    logger.info(s"Messages to Anton ${sbaAnton.size} of type ${SETTLE_BID_ACTION}")
    Thread.sleep(100)
    sbaAnton foreach {println}
    logger.info(s"Messages to 1665771990162012 ${btaSaladSpinner.size} of type $BID_TAKE_ACTION")
    Thread.sleep(100)
    btaSaladSpinner foreach println


    countAsba.keys.size should be(as.size * as.size)
    countSbaTo forall {_._2.size == bids.size} shouldBe true
    countBtaTo forall {_._2.size == bids.size} shouldBe true
    countAsba filterNot {_._2.size == bids.size} shouldBe empty

    MockSendReceiveInterface.log.count { l ⇒
      l.payloadId == RETRACT_BID_ACTION || l.payloadId == REJECT_BID_ACTION || l.payloadId == DENY_SETTLE_BID_ACTION
    } should be(0)
  }

  def printGrouped(what: Map[_, Iterable[_]]) = what foreach { g ⇒
    println(g._1)
    g._2 foreach { m ⇒ println("\t" + m) }
  }
}