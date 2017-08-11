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
    val agents = StateManager.loadAll() foreach Network.registerAgent
    FbAgent.load()
    FbServer.start()
  }
}
