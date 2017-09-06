import java.util.concurrent.ForkJoinPool

import com.restfb.{DefaultFacebookClient, Version}

import scala.concurrent.ExecutionContext

/**
  * Created by anton on 8/11/17.
  */
package object fb {
  val fbClient = new DefaultFacebookClient(FbSettings.pageToken, Version.VERSION_2_9)
  val thanksSymbol: Char = '\u20B8'
}
