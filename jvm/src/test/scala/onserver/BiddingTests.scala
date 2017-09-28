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
import plenty.state.StateManager
import plenty.state.model._

import scala.language.postfixOps

class BiddingTests extends FreeSpec with Matchers {
  private val logger = Logger.getAnonymousLogger()

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

    val bids = List(Bid("uZiKsfbooqAvC3LLhi9r5kqxD4VHJPs7N9hl6jSRTQCaEacWGiiN6hMtIdREPYE_gDg2xDH1V9dpVaej_Wxsqw",
      Donation("269448170223322_287661961735276", Some("Salad spinner"), Some("A salad enthusiast. Huge salads is the" +
        " " +
        "meaning of life."), Some("This spinner makes it easy NOT to end up with a bunch of water in your salad. It " +
        "gets almost all the water off the greens you put in it. You can also hack it, and use it as a lazy salad " +
        "greens washer -- fill with water and spin."), Some("Etobicoke. Possibly downtown."), Some("Meet or pickup " +
        "during the day. I'm usually in Etobicoke, but some days I come downtown."), Some("You have to have fun using" +
        " it! Spin it, spin it, spin it... :)"), Some("It's too small for the huge salads that we make"), Vector
      ("https://scontent-atl3-1.xx.fbcdn.net/v/t35.0-12/21754933_10212967930569323_150718313_o" +
        ".jpg?_nc_ad=z-m&oh=825f73f68cb6106c79d4ac6cbd106d99&oe=59BC2BB3", "https://scontent-atl3-1.xx.fbcdn" +
        ".net/v/t35.0-12/21754686_10212967930369318_673361208_o" +
        ".jpg?_nc_ad=z-m&oh=e9098cb3de4f31260cb19407228e3578&oe=59BB55B8"), Node("767613720030082"), 1505328447641L),
      1, Node("1665771990162012"), 1506150238033L), Bid
    ("gjyXXj4Z9WWG5a06IQDjIXhMIkiCLMPmqr1ywGWM5u-ayT3H0YVfqRoh_G-NLiNelIigoIXGcuS8Cltt7U_HSw", Donation
    ("269448170223322_287670738401065", Some("Baking set"), Some("A cook with a tiny kitchen."), Some("A beautiful " +
      "ceramic red baking set. 1 large pan, 2 medium, and 2 small. Fitting nicely inside each other. One medium pan " +
      "has a crack in it, but it doesn't seem to be deep, doesn't feel like it will fall apart anytime soon."), Some
    ("Etobicoke. Possibly downtown."), Some("During the day"), Some("If my heart breaks when I'm giving the set away," +
      " please call the ambulance."), Some("No space :( Our kitchen is tiny"), Vector("https://scontent-atl3-1.xx" +
      ".fbcdn.net/v/t35.0-12/21742312_10212968094093411_347634509_o" +
      ".jpg?_nc_ad=z-m&oh=964b52d402aa562f2e159c4e5b5d0883&oe=59BB458C", "https://scontent-atl3-1.xx.fbcdn.net/v/t35" +
      ".0-12/21742309_10212968094653425_1050557196_o.jpg?_nc_ad=z-m&oh=1822bbbd315901f2ccab26db2291c0cf&oe=59BC394E")
      , Node("767613720030082"), 1505329582528L), 5, Node("1528997860491908"), 1506012403508L), Bid
    ("3fRe9wjQPPoiX4mje2M8ydF-xS_zSiy-muXWYE72ZrFENCflwk6GL9u9vBbjjHC3Qto3ULv_nTvUKIHYd3d-qA", Donation
    ("269448170223322_289359578232181", Some("Handmade clay plate set"), Some("This set is made by my mother in law. " +
      "She has been practising pottery for years, and this set is from the time when she has become good at it."),
      Some("Handmade pottery. 6 plates and a saucer. This is the best set that I have."), Some("Toronto, downtown or " +
      "Etobicoke"), Some("During the day / early evening"), Some("We can arrange to meet, but pickup is preferred"),
      Some("I have quite a few handmade plates, bowls and saucers, and they are just collecting dust"), Vector
    ("https://scontent.xx.fbcdn.net/v/t35.0-12/21640570_10212983136989474_1219141600_o" +
      ".jpg?_nc_ad=z-m&_nc_cid=0&oh=1c86e92304f528c008f40affbbfefc7e&oe=59BDE890", "https://scontent.xx.fbcdn" +
      ".net/v/t35.0-12/21682763_10212983137629490_1688152105_o" +
      ".jpg?_nc_ad=z-m&_nc_cid=0&oh=cecfb99b11e25b5ff3b19b98c6f29e90&oe=59BDCEF2"), Node("767613720030082"),
      1505504154540L), 7, Node("1495520050542318"), 1506099166561L))
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
    SETTLE_BID_ACTION.setLoggingFunction((t) ⇒ s"\n\t${t.bid.donation.title.get}")
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