package fb.network

import java.util.logging.Logger

import com.restfb.types.webhook.{Change, FeedCommentValue, WebhookEntry}
import fb.{FbAgent, Responses}
import plenty.state.StateManager

object CommentWatcher {
  private val logger = Logger.getLogger("CommentWatcher")

  def receiveWebhook(webhookEntry: WebhookEntry): Unit = {
    val i = webhookEntry.getChanges.iterator()
    while (i.hasNext) {
      processChange(i.next())
    }
  }

  private def processChange(c: Change) = {
    c.getValue match {
      case c: FeedCommentValue ⇒ processComment(c)
      case _ ⇒
    }
  }

  private def processComment(c: FeedCommentValue) = {
    val s = FbAgent.lastState
    StateManager.getDonation(c.getPostId)(s) match {
      case Some(d) ⇒ Responses.notifyOfComment(d, c.getMessage, c.getCommentId)
      case None ⇒ logger.fine(s"Could not find donation with id ${c.getPostId}")
    }
  }
}
