package plenty.state

import plenty.state.model.Donation

object DonationStateUtils {

  // fixme put create donation here as well

  /** tells if a field is filled from donation given a field name
    * @note `pictures` field can never be filled */
  def isFieldFilled(d: Donation, name: String): Boolean = name match {
    case "title" ⇒ getTextFieldByName(d, name).nonEmpty
    case "who" ⇒ getTextFieldByName(d, name).nonEmpty
    case "when" ⇒ getTextFieldByName(d, name).nonEmpty
    case "what" ⇒ getTextFieldByName(d, name).nonEmpty
    case "where" ⇒ getTextFieldByName(d, name).nonEmpty
    case "why" ⇒ getTextFieldByName(d, name).nonEmpty
    case "how" ⇒ getTextFieldByName(d, name).nonEmpty
    case "first_picture" ⇒ getTextFieldByName(d, name).nonEmpty
    case "pictures" ⇒ false
  }

  /** gets a text field value by name from a donation */
  def getTextFieldByName(d: Donation, name: String): Option[String] = ???
  //  name match {
  //    case "title" ⇒ d.title
  //    case "who" ⇒ d.who
  //    case "when" ⇒ d.when
  //    case "what" ⇒ d.what
  //    case "where" ⇒ d.where
  //    case "why" ⇒ d.why
  //    case "how" ⇒ d.how
  //    case "first_picture" ⇒ d.attachments.headOption
  //  }

  /** replaces the value with given value in donation field with given name
    * @return copy of donation */
  def updateField(d: Donation, name: String, value: Option[String]): Donation = ???
  //    name match {
  //    case "title" ⇒ d.copy(title = value)
  //    case "who" ⇒ d.copy(who = value)
  //    case "when" ⇒ d.copy(when = value)
  //    case "what" ⇒ d.copy(what = value)
  //    case "where" ⇒ d.copy(where = value)
  //    case "why" ⇒ d.copy(why = value)
  //    case "how" ⇒ d.copy(how = value)
  //  }
}
