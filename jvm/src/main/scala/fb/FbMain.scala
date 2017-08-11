package fb

import com.restfb.{DefaultFacebookClient, Version}
import plenty.agent.AgentPointer
import plenty.network.Network
import plenty.state.StateManager

/**
  * The main entry for FB code
  */
object FbMain {
  def main(args: Array[String]): Unit = {

    // loading network
    FbAgent.load()
    val agents = StateManager.loadAll() foreach Network.registerAgent

    FbServer.start()
  }
}
