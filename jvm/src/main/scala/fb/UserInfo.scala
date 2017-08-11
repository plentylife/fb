package fb

import com.restfb.Parameter
import com.restfb.types.User

/**
  * Created by anton on 8/11/17.
  */
object UserInfo {
  private var store = Map[String, UserInfo]()

  def add(userInfo: UserInfo) = synchronized {
    store += userInfo.id -> userInfo
  }

  def get(id: String): UserInfo = store.get(id) match {
    case Some(ui) => ui
    case _ => retrieveUserInfo(id)
  }

  def retrieveUserInfo(id: String): UserInfo = {
    val fbUser = fbClient.fetchObject(id, classOf[User], Parameter.`with`("fields", "first_name"))
    val ui = UserInfo(id, name = fbUser.getFirstName)
    add(ui)
    ui
  }
}

case class UserInfo(id: String, name: String)
