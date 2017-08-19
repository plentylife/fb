package fb

import plenty.agent.Scheduler
import plenty.network.Network
import plenty.state.StateManager

/**
  * The main entry for FB code
  */
object FbMain {
  def main(args: Array[String]): Unit = {

    // loading network
    StateManager.loadAll() foreach { a => Network.registerAgent(a, FbSendInterface) }

    Scheduler.start()

    FbAgent.load()
    FbServer.start()
  }
}
