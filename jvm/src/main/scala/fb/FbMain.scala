package fb

import java.util.logging.{Level, LogManager}

import plenty.agent.Scheduler
import plenty.network.Network
import plenty.state.StateManager

/**
  * The main entry for FB code
  */
object FbMain {
  def main(args: Array[String]): Unit = {
    LogManager.getLogManager.readConfiguration(FbMain.getClass.getResourceAsStream("/logging.properties"))

    // loading network
    StateManager.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }

    Scheduler.start()

    // loading coins as well
    FbAgent.load()

    if (FbSettings.prod) FbServer.start()
    else FbServer.startAndWait()
  }
}
