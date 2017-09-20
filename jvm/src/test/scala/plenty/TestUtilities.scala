package plenty

import java.util.Date

import fb.UserInfo
import plenty.agent.model.Agent
import plenty.network.{Message, Network, SendReceiveInterface}
import plenty.state.model.Transaction

object TestUtilities {
  def getAgents: Iterable[Agent] = {
    Network.getAgents.map(_.agentInLastState)
  }

  def waitClearQueue(printMsg: Boolean = false) = {
    println("waiting on message queue")
    println(s"non-completes: ${Network.nonCompletes.keySet.mkString(" ")}")
    while (Network.nonCompletes.nonEmpty) {
      Thread.sleep(1000)
      if (!printMsg)
        println(s"non-completes (loop): ${Network.nonCompletes.keySet.mkString(" ")}")
      else
        println(s"non-completes (loop): ${Network.nonCompletes.mkString("\n")}")
    }
  }

  def getName(id: String) = id match {
    case "facebook_agent" ⇒ id
    case id ⇒ UserInfo.get(id).name
  }

  def pprint(t: Transaction) = {
    val nameFrom = getName(t.from.id)
    val nameTo = getName(t.to.id)
    println(s"${nameFrom} -> ${nameTo} " +
      s"${t.coins.size}\t ${t.transactionType} \t${new Date(t.timestamp)}")
  }

  def div(mark: String = "") = println(s"\n\n===\t$mark\n")

  def transactionsInvloving(a: Agent) = a.state.chains.transactions.filter(t ⇒ {
    t.from == a.node || t.to == a.node
  })
}

object MockSendReceiveInterface extends SendReceiveInterface {
  var logging = true

  var log = List[Message[_]]()

  private def addToLog(msg: Message[_]) = synchronized {
    log = msg +: log
  }

  override def send(msg: Message[_]): Unit = {
    if (logging) println(msg)
    addToLog(msg)
    Network.receive(msg)
  }
}