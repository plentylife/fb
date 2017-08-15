package fb

import com.restfb.types.{Account, GraphResponse}
import com.restfb.{DefaultFacebookClient, Parameter, Version}
import plenty.agent.{AgentManager, AgentPointer}
import plenty.network.Network
import plenty.state.StateManager

/**
  * The main entry for FB code
  */
object FbMain {
  def main(args: Array[String]): Unit = {

    AgentManager.callbacks = NetworkCallbacks
    // loading network
    val agents = StateManager.loadAll() foreach Network.registerAgent

    FbAgent.load()

    println(s"publishing to ${AccessTokens.pageId} with token ${AccessTokens.pageToken}")
    val publishMessageResponse = fbPageClient.publish(s"${AccessTokens.pageId}/feed",
      classOf[GraphResponse], Parameter.`with`("message", "test"))
    println("Published message ID: " + publishMessageResponse.getId)


    FbServer.start()
  }
}
