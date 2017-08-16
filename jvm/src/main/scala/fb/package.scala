import com.restfb.types.webhook.messaging.MessagingItem
import com.restfb.{DefaultFacebookClient, Version}
import plenty.agent.model.Agent
import plenty.state.StateManager
import plenty.state.model.{Donation, Node}

/**
  * Created by anton on 8/11/17.
  */
package object fb {
  val fbMsgClient = new DefaultFacebookClient(AccessTokens.pageToken, Version.VERSION_2_9)
  val fbPageClient = new DefaultFacebookClient(AccessTokens.pageToken, Version.VERSION_2_9)

}
