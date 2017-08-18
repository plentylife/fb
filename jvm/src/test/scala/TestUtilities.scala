import plenty.agent.model.Agent
import plenty.network.Network

object TestUtilities {
  def getAgents: Iterable[Agent] = {
    Network.getAgents.map(_.getAgentInLastKnownState)
  }

  def waitClearQueue = {
    println("waiting on message queue")
    println(s"non-completes: ${Network.nonCompletes.mkString(" ")}")
    while (Network.nonCompletes.nonEmpty) {
      Thread.sleep(1000)
      println(s"non-completes (loop): ${Network.nonCompletes.mkString(" ")}")
    }
  }
}