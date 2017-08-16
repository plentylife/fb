package fb

import java.util

import com.restfb.types.Message.Attachment
import com.restfb.types.send.MediaAttachment
import com.restfb.types._
import com.restfb.{DefaultFacebookClient, Parameter, Version}
import plenty.agent.{AgentManager, AgentPointer}
import plenty.network.Network
import plenty.state.StateManager

/**
  * The main entry for FB code
  */
object FbMain {
  def main(args: Array[String]): Unit = {

    // loading network
    StateManager.loadAll() foreach {a => Network.registerAgent(a, FbSendInterface)}

    FbAgent.load()
    FbServer.start()
  }
}
