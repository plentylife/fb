package fb

import java.util.Date

import com.restfb.Parameter
import com.restfb.types.UserProfile

/**
  * Created by anton on 8/11/17.
  */
object UserInfo {
  private var store = Map[String, UserInfo]()

  def add(userInfo: UserInfo) = synchronized {
    store += userInfo.id -> userInfo
  }

  def get(id: String): UserInfo = store.get(id) match {
    case Some(ui) =>
      // updating last access
      add(ui.copy(lastAccess = new Date().getTime))
      ui
    case _ => retrieveUserInfo(id)
  }

  def retrieveUserInfo(id: String): UserInfo = {
    val fbUser = fbClient.fetchObject(id, classOf[UserProfile], Parameter.`with`("fields", "first_name, " +
      "last_name, profile_pic"))
    val ui = UserInfo(id, name = fbUser.getFirstName, lastName = fbUser.getLastName, fbUser.getProfilePic)
    add(ui)
    ui
  }
}

case class UserInfo(id: String, name: String, lastName: String, profilePic: String,
                    lastAccess: Long = new Date().getTime)
