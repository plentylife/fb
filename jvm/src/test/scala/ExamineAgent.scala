import fb.{FbAgent, FbSettings, UserInfo}
import org.scalatest.FreeSpec
import plenty.agent.Accounting
import plenty.state.StateManager
import plenty.TestUtilities._

import scala.language.postfixOps

class ExamineAgent extends FreeSpec {

  "Single agent" in {
//    val agent = StateManager.load("1495520050542318", "onserver/") // gyorgy
    val agent = StateManager.load("1624828950901835", "onserver/")

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
    val agent = StateManager.load("facebook_agent")

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
}
