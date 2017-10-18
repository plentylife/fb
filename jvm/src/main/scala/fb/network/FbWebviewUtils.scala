package fb.network

import java.util

import com.restfb.Parameter
import com.restfb.types.{Comment, Post}
import io.circe.Encoder

import scala.language.{implicitConversions, postfixOps}

object FbWebviewUtils {
  private val commentFields = new util.ArrayList[String]()
  Array("attachment", "message", "created_time", "from", "comments") foreach commentFields.add

  def getPost(postId: String): Option[FbPost] = {
    getPostBody(postId) map { body ⇒
      FbPost(body, getPostComments(postId))

    }
  }

  def getPostBody(postId: String): Option[String] = {
    val p = fb.fbClient.fetchObject(postId, classOf[Post])
    Option(p) map {_.getMessage}
  }

  def getPostComments(postId: String): List[FbComment] = {
    val con = fb.fbClient.fetchConnection(s"$postId/comments", classOf[Comment],
      Parameter.`with`("fields", commentFields))
    val iter = con.iterator()

    var res = List[FbComment]()
    while (iter.hasNext) {
      val page = iter.next()
      page.toArray foreach { case c: Comment ⇒
        val fbc: FbComment = c
        val replies = Option(c.getComments).map {_.getData}.map {_.toArray()}.map { rarr: Array[AnyRef] ⇒
          rarr.collect { case r: Comment ⇒ r: FbComment } toList
        } getOrElse {List[FbComment]()}
        res = fbc.copy(replies = replies) :: res
      }
    }
    res
  }

  private implicit def commentConv(c: Comment): FbComment = {
    val body = c.getMessage
    val date = c.getCreatedTime
    val attch = c.getAttachment
    val from = c.getFrom.getName
    FbComment(body, from, attachments = Option(attch).map(_.getUrl()), date = date.getTime)
  }

  import io.circe.generic.semiauto._

  implicit val encoderPost: Encoder[FbPost] = deriveEncoder[FbPost]
  implicit val encoderComment: Encoder[FbComment] = deriveEncoder[FbComment]
}

case class FbComment(body: String, from: String, replies: List[FbComment] = List(), attachments: Option[String],
                     date: Long)

case class FbPost(body: String, comments: List[FbComment])
