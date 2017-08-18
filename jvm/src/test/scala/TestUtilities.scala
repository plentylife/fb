import plenty.agent.model.Agent
import plenty.network.Network

object TestUtilities {
  def getAgents: Iterable[Agent] = {
    Network.getAgents.map(_.getAgentInLastKnownState)
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