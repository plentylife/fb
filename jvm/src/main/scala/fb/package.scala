import com.restfb.{DefaultFacebookClient, Version}

/**
  * Created by anton on 8/11/17.
  */
package object fb {
  val fbClient = new DefaultFacebookClient(AccessTokens.pageToken, Version.VERSION_2_9)
}
