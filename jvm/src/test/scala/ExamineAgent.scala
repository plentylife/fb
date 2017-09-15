import fb.{FbAgent, FbSettings, UserInfo}
import org.scalatest.{FreeSpec, Matchers}
import plenty.agent.Accounting
import plenty.state.StateManager
import plenty.TestUtilities._
import plenty.network.MintPress

import scala.language.postfixOps

class ExamineAgent extends FreeSpec with Matchers {

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
    (agent.state.coins filter {c ⇒ myOldCoinIds contains c.id}).foreach(println)
    val transactionsThatContainMyCoins = agent.state.chains.transactions
      .map(t ⇒ t → (t.coins intersect myOldCoins)).filter(_._2.nonEmpty).map(_._1)
    println("trans with my coins")
    transactionsThatContainMyCoins foreach {t ⇒
      pprint(t); println(t.id); println(t.coins.map{_.id});println(t.transactionType);println()}
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
    val fbAgent = as find(_.id == "facebook_agent") get;

    as filterNot {_.id == "facebook_agent"} foreach { a =>
      val ui = UserInfo.get(a.id)
      val balanceSelf = Accounting.getBalance(a.node)(a)
      val balanceFb = Accounting.getBalance(a.node)(fbAgent)

      println(s"\t ${a.id}")
      println(s"${ui.name} ${balanceSelf} $balanceSelf")
    }
  }

  "Modifying server agents" - {
    import com.softwaremill.quicklens._
    FbSettings.prod = true
    val as = StateManager.loadAll("onserver/")

    as filterNot (_.id == "facebook_agent") foreach {a ⇒
      val ui = UserInfo.get(a.id)
      println(ui.name, a.id)
    }

    "Removing bids" in {
      div("bids")
      as foreach {a ⇒
        println(a.state.bids)
        val u = a.modify(_.state.bids).using(_.filterNot(_.by.id == "1624828950901835"))
        StateManager.save(u, "onserver/")
      }
      Thread.sleep(100)
    }

    "Removing transactions" in {
      div("transactions")
      as foreach {a ⇒
        println(s"\n${a.id}")
        a.state.chains.transactions.foreach {pprint}
        val u = a.modify(_.state.chains.transactions).using(_ ⇒ List())
        StateManager.save(u, "onserver/")
      }
      Thread.sleep(100)
    }

    "Adding coins" in {
      div("minting")
      val allCoins = MintPress.fillCoinSet(Set(), null).grouped(7).toList

      val distributedCoins = as filterNot (_.id == "facebook_agent") zip allCoins flatMap {case (a, cs) ⇒
        cs map {_.modify(_.belongsTo).using(_ ⇒ a.node)}
      }

      distributedCoins foreach println

      as foreach {a ⇒
        val u = a.modify(_.state.coins).using(_ ⇒ distributedCoins)
        StateManager.save(u, "onserver/")
      }

      Thread.sleep(100)
    }

    "Checking coins" in {
      div("coin check")
      val check = StateManager.loadAll("onserver/")

      check filterNot (_.id == "facebook_agent") foreach {a1 ⇒
        check foreach {a2 ⇒
          val b = Accounting.getBalance(a1.node)(a2)
          println(b)
          b shouldBe 7
        }
      }
    }


  }
}
