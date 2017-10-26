package plenty.state

import plenty.state.model.{Donation, State}

object Search {

  /** splits the `what` parameter by space, looks for donations that contain tokens that in turn contain search terms.
    * the results are ranked based on the number of search terms present, and whether the tokens are tagged. */
  def searchDescriptionsByString(what: String, state: State): List[Donation] = {
    val tokens = what split " "

    val hasTokens = state.donations map { d ⇒
      val present = tokens flatMap { t ⇒ getToken(d, t) }
      d → present
    } filter {_._2.nonEmpty} toList

    val sorted = hasTokens map { case (d, present) ⇒
      val score = present.toList map { t ⇒ if (t.isTagged) 2 else 1 } sum;
      d -> score
    } sortBy (_._2) map {_._1} reverse;

    sorted
  }

  private def getToken(d: Donation, t: String) = d.description find (_.token contains t)
}
