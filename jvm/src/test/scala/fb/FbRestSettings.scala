package fb

import org.scalatest.FreeSpec

/**
  * Saving state, modifying state by agents
  */
class FbRestSettings extends FreeSpec {
  FbSettings.prodOverride = Option(true)
  val antonId = "1783146675033183"

  "Get whitelisted domains" - {
    val command =
      s"""curl -X GET "https://graph.facebook.com/v2.6/me/messenger_profile?fields=whitelisted_domains&access_token=${FbSettings.pageToken}" """

    println(command)

    println()
//    println(command.!!)

  }

  "Delete" - {
    val command = s"""curl -X DELETE -H "Content-Type: application/json" -d '{
                    |  "fields":[
                    |    "whitelisted_domains"
                    |  ]
                    |}' "https://graph.facebook.com/v2.6/me/messenger_profile?access_token=${FbSettings.pageToken}"
                    |""".stripMargin('|')

    println(command)
    println()
  }

  "List" - {
    val command = s"""curl -X POST -H "Content-Type: application/json" -d '{
                    |  "whitelisted_domains":[
                    |    "https://plenty.life"
                    |  ]
                    |}' "https://graph.facebook.com/v2.6/me/messenger_profile?access_token=${FbSettings.pageToken}" """
      .stripMargin
    println(command)
  }

  "get started button" - {

  }
}
