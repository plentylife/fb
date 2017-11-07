package fb.network

import java.util

import com.restfb.Parameter
import com.restfb.types.{Comment, Photo, Post, StoryAttachment}
import fb.FbSettings
import io.circe.Encoder
import plenty.state.model.Donation

import scala.language.{implicitConversions, postfixOps}

object FbWebviewUtils {
  private val commentFields = new util.ArrayList[String]()
  private val pictureFields = new util.ArrayList[String]()
  Array("attachment", "message", "created_time", "from", "comments") foreach commentFields.add
  pictureFields.add("picture")

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
        res = fbc.copy(comments = replies) :: res
      }
    }
    res
  }

  def getPictureUrl(fbid: String): Option[String] = {
    val p = fb.fbClient.fetchObject(fbid, classOf[Photo], Parameter.`with`("fields", pictureFields))
    Option(p).map(_.getPicture)
  }

  def viewDonationUrl(d: Donation) = s"${FbSettings.webviewViewPath}/donation/${d.id}"

  def createDonationUrl = s"${FbSettings.webviewViewPath}/donation/create"

  def searchDonationUrl = s"${FbSettings.webviewViewPath}/search"

  def fallbackPageUrl = s"${FbSettings.webviewViewPath}/fallback"

  def makeUriGlobal(o: String) = s"${FbSettings.baseUri}$o"

  private implicit def commentConv(c: Comment): FbComment = {
    val body = c.getMessage
    val date = c.getCreatedTime
    val attch = c.getAttachment
    val picture = Option(attch) flatMap extractPicture
    val from = c.getFrom.getName
    FbComment(body, from, attachments = picture, date = date.getTime)
  }

  private def extractPicture(s: StoryAttachment): Option[String] = {
    Option(s.getId) match {
      case Some(id) ⇒ getPictureUrl(id)
      case None ⇒ Option(s.getMedia) flatMap { m ⇒ Option(m.getImage) } map {_.getSrc}
    }
  }

  import io.circe.generic.semiauto._

  implicit val encoderPost: Encoder[FbPost] = deriveEncoder[FbPost]
  implicit val encoderComment: Encoder[FbComment] = deriveEncoder[FbComment]
}

case class FbComment(body: String, from: String, comments: List[FbComment] = List(), attachments: Option[String],
                     date: Long)

case class FbPost(body: String, comments: List[FbComment])