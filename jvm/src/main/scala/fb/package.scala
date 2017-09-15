import java.util.logging.{Level, LogManager}

import com.restfb.{DefaultFacebookClient, Version}

/**
  * Created by anton on 8/11/17.
  */
package object fb {
  val fbClient = new DefaultFacebookClient(FbSettings.pageToken, Version.VERSION_2_9)
  val thanksSymbol: Char = '\u20B8'
}
