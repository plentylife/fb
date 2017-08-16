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
    val agents = StateManager.loadAll() foreach Network.registerAgent

//    val img = new Attachment()
//    img.setFileUrl("http://st.depositphotos.com/1005920/4362/i/950/depositphotos_43629535-stock-photo-test-icon.jpg")
//    val msg = new Message()
//    msg.setMessage("this message is with images")
//    msg.addAttachment(img)
//
//    val ppr1 = fbPageClient.publish(s"${AccessTokens.pageId}/photos",
//      classOf[Photo],
//      Parameter.`with`("url", "http://st.depositphotos" +
//        ".com/1005920/4362/i/950/depositphotos_43629535-stock-photo-test-icon.jpg"),
//      Parameter.`with`("published", true)
//    )
//    val ppr2 = fbPageClient.publish(s"${AccessTokens.pageId}/photos",
//      classOf[Photo],
//      Parameter.`with`("url", "https://images.duckduckgo.com/iu/?u=https%3A%2F%2Ffbblogmessenger.files.wordpress.com%2F2016%2F09%2F02-native-payments.gif%3Fw%3D213%26h%3D435&f=1")
//      ,
//      Parameter.`with`("published", false)
//    )
//
//    println(ppr1)
//    println()
//    println(ppr2)
//
//    val mat = new util.ArrayList[String]()
//    mat.add(s"{'media_fbid':'${ppr1.getId}'}")
//    mat.add(s"{'media_fbid':'${ppr2.getId}'}")
//    val ppm = fbPageClient.publish(s"${AccessTokens.pageId}/feed",
//      classOf[Post],
//      Parameter.`with`("message", "will it attach?"),
//      Parameter.`with`("attached_media", mat)
//    )
//    println(s"pmr ${ppm} id ${ppm.getId}")

    FbAgent.load()
    FbServer.start()
  }
}
