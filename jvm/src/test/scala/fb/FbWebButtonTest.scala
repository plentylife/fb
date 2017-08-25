package fb

import java.util.Date

import com.restfb.Parameter
import com.restfb.types.send._
import org.scalatest.FeatureSpec

/**
  * Saving state, modifying state by agents
  */
class FbWebButtonTest extends FeatureSpec {
  FbSettings.prod = true
  // test
//  val antonId  = "1783146675033183"
  // prod
  val anton = "767613720030082"
  val andrey = "1624828950901835"
  val toId = anton

  info("The webview should be shareable")

  feature("message with webview and share buttons") {
    scenario("the message is sent") {
      val recipient = new IdMessageRecipient(toId)
      val template = new GenericTemplatePayload()
      val bubble = new Bubble(s"webview share ${new Date().getTime}")
      val url = "https://plenty.life/"
      val button = new WebButton(s"test", url)
      val mmeButton = new WebButton("m.me", s"m.me/${FbSettings.pageId}")

      val share = new ShareButton()

      button.setWebviewHeightRatio(WebviewHeightEnum.full)
      button.setMessengerExtensions(true, url)
      bubble.addButton(button)
      bubble.addButton(share)
      bubble.addButton(mmeButton)
      template.addBubble(bubble)

      fbClient.publish("me/messages", classOf[SendResponse], Parameter.`with`("recipient", recipient),
        Parameter.`with`("message", new Message(new TemplateAttachment(template)))
      )
    }
  }

//  feature("shareble postbacks") {
//    scenario("message is sent, then shared") {
//      val recipient = new IdMessageRecipient(toId)
//      val template = new GenericTemplatePayload()
//      val bubble = new Bubble(s"postback share ${new Date().getTime}")
//      val button = new PostbackButton("post", "DONATE_START_POSTBACK")
//      val share = new ShareButton()
//
//      bubble.addButton(button)
//      bubble.addButton(share)
//      template.addBubble(bubble)
//
//      fbClient.publish("me/messages", classOf[SendResponse], Parameter.`with`("recipient", recipient),
//        Parameter.`with`("message", new Message(new TemplateAttachment(template)))
//      )
//    }
//  }

//  FbMain.main(Array())
}
