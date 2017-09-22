import com.softwaremill.quicklens._
import fb.donation.DonationResponses
import fb.{FbSettings, UserInfo}
import org.scalatest.{FreeSpec, Matchers}
import plenty.TestUtilities._
import plenty.agent.{Accounting, AgentPointer}
import plenty.network.MintPress
import plenty.state.StateManager
import plenty.state.model.{BidTransaction, Node}

import scala.language.postfixOps

class ExamineAgent extends FreeSpec with Matchers {

  "Analyzing agents" - {
    "Single agent" in {
      //    val agent = StateManager.load("1495520050542318", "onserver/") // gyorgy
      val agent = StateManager.load("1624828950901835", "onserver/").get

      println(s"all coins count ${agent.state.coins.size}")
      println(s"my coin coint is ${Accounting.getOwnCoins(agent)}")
      println()
      val transWithMe = agent.state.chains.transactions.filter(t ⇒ t.to == agent.node || t.from == agent.node)
      transWithMe.foreach(t ⇒ pprint(t))
      println()
      println("coins that used to belong to me")
      val myOldCoins = transWithMe.head.coins intersect agent.state.coins
      val myOldCoinIds = myOldCoins map {_.id}
      (agent.state.coins filter { c ⇒ myOldCoinIds contains c.id }).foreach(println)
      val transactionsThatContainMyCoins = agent.state.chains.transactions
        .map(t ⇒ t → (t.coins intersect myOldCoins)).filter(_._2.nonEmpty).map(_._1)
      println("trans with my coins")
      transactionsThatContainMyCoins foreach { t ⇒
        pprint(t); println(t.id); println(t.coins.map {_.id}); println(t.transactionType); println()
      }
    }

    "FB Agent" in {
      println("\n\n")
      val agent = StateManager.load("facebook_agent").get

      println("coins that do not belong to fb agent")
      val notMine = agent.state.coins.filterNot(_.belongsTo == agent.node)
      notMine foreach println
      println(s"coins that do ${agent.state.coins.count(_.belongsTo == agent.node)}")

      println(agent.state.coins)
    }

    "All server agents" in {
      FbSettings.prod = true

      val as = StateManager.loadAll("onserver/")
      val fbAgent = as find (_.id == "facebook_agent") get;

      println(s"Fb agent has ${Accounting.getSelfBalance(fbAgent)}")

      as filterNot {_.id == "facebook_agent"} foreach { a =>

        val ui = UserInfo.get(a.id)
        val balanceSelf = Accounting.getBalance(a.node)(a)
        val balanceFb = Accounting.getBalance(a.node)(fbAgent)

        div()
        println(s"${a.id} \t ${ui.name} $balanceSelf $balanceFb")

        transactionsInvloving(a) foreach pprint
        //        transactionsInvloving(a) filterNot (_.transactionType == TransactionType.DEMURAGE) foreach pprint
      }

      div("donations")
      as flatMap {_.state.donations} foreach { d ⇒
        println(d.title)
        println(d.attachments mkString ("\n"))
      }
    }
  }

  "See all bids" in {
    val as = StateManager.loadAll("onserver/")

    as filterNot (_.id == "facebook_agent") foreach { a ⇒
      val ui = UserInfo.get(a.id)
      println(ui.name, a.id)
    }

    val bids = as flatMap {_.state.bids}

    bids foreach println
  }

  "Using server agents" - {
    "Send bubbles" in {
      FbSettings.prod = true
      val as = StateManager.loadAll("onserver/")
      val antonNode = Node("767613720030082")

      as find (_.node == antonNode) foreach { a ⇒
        val p = new AgentPointer(a)
        a.state.donations find {_.title.get contains "clay"} foreach { d ⇒
          println(d)
          DonationResponses.showDonationBubble(p, d, Some(d.id))
        }
      }
    }
  }

  "Modifying server agents" - {
    FbSettings.prod = true
    var as = StateManager.loadAll("onserver/")

    "Outputting basic info" in {
      as filterNot (_.id == "facebook_agent") foreach { a ⇒
        val ui = UserInfo.get(a.id)
        println(ui.name, a.id)
      }
    }

    "Adding coins" in {
      val allCoins = MintPress.fillCoinSet(Set(), null).grouped(7).toList

      val distributedCoins = as filterNot (_.id == "facebook_agent") zip allCoins flatMap { case (a, cs) ⇒
        cs map {_.modify(_.belongsTo).using(_ ⇒ a.node)}
      }

      div("minting")
      println(distributedCoins mkString ("\n"))

      as foreach { a ⇒
        val u = a.modify(_.state.coins).using(_ ⇒ distributedCoins)
        StateManager.save(u, "onserver/")
      }

    }

    "Checking coins" in {
      div("coin check")
      val check = StateManager.loadAll("onserver/")

      check filterNot (_.id == "facebook_agent") foreach { a1 ⇒
        check foreach { a2 ⇒
          val b = Accounting.getBalance(a1.node)(a2)
          b shouldBe 7
        }
      }

      Accounting.getSelfBalance({check find (_.id == "facebook_agent") get}) should be(0)
    }


    "making sure that the correct transactions are kept" in {
      as = StateManager.loadAll("onserver/")

      val antonNode = Node("767613720030082")
      val sarahNode = Node("1843592592335659")
      // selecting bid transactions that should be kept
      val bidTransactions = as flatMap {_.state.chains.transactions} collect { case t: BidTransaction ⇒ t }

      val sarahsBuy = bidTransactions filter { t ⇒
        t.from.id == "1843592592335659" && t.to.id == "767613720030082" &&
          t.coins.size == 3
      }
      sarahsBuy should have size (1)

      div("sarahs transaction")
      pprint(sarahsBuy.head)

      var transactionToKeep = sarahsBuy.head
      val sarahsCoins = Accounting.getOwnCoins(
        {as find (_.node == transactionToKeep.from) get}) take transactionToKeep.coins.size
      sarahsCoins forall {_.belongsTo == sarahNode} shouldBe true
      transactionToKeep = transactionToKeep.modify(_.coins).using(_ ⇒ sarahsCoins)
      val antonsCoins = Accounting.transferCoins(transactionToKeep)

      transactionToKeep.coins forall {_.belongsTo == sarahNode} shouldBe true
      antonsCoins forall (_.belongsTo.id == "767613720030082") shouldBe true

      div("coin update")
      as foreach { a ⇒
        println("modifying", a.id)
        var upd = a.modify(_.state.coins).using { c ⇒
          val stage1 = c diff antonsCoins
          stage1 ++ antonsCoins
        }
        upd = upd.modify(_.state.chains.transactions).using(_ ⇒ List(transactionToKeep))
        StateManager.save(upd, "onserver/")
      }

      var asCheck = StateManager.loadAll("onserver/")
      asCheck forall { a ⇒
        println(a.id, Accounting.getBalance(antonNode)(a))
        Accounting.getBalance(antonNode)(a) == 10
      } shouldBe true
      asCheck forall { a ⇒ Accounting.getBalance(sarahNode)(a) == 4 } shouldBe true
      val others = asCheck filterNot { a ⇒ a.node == antonNode || a.node == sarahNode || a.id == "facebook_agent" }
      others forall {
        a ⇒ Accounting.getSelfBalance(a) == 7
      } shouldBe true
      asCheck forall {
        _.state.chains.transactions.exists { t ⇒
          t.from == sarahNode && t.to == antonNode && t.coins.size == 3
        }
      } shouldBe true
    }
  }
}
