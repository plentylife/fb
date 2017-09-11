import plenty.agent.model.Agent
import plenty.network.{Message, Network, SendReceiveInterface}

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