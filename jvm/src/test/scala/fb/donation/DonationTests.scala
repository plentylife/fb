package fb.donation

import org.scalatest.FreeSpec
import plenty.agent.AgentPointer
import plenty.agent.model.Agent
import plenty.state.StateManager
import plenty.state.model.{Node, State}

class DonationTests extends FreeSpec {

  "Publishing of donation posts on page" - {
    // dev
    val anton = "1783146675033183"
    // prod
    //    val anton = "767613720030082"
    val andrey = "1624828950901835"
    val toId = anton

    val node = Node(toId)
    val a = Agent(toId, State())
    val ap = new AgentPointer(a)

    "with all fields filled out" - {
      val donation = StateManager.createEmptyDonation(by = node)
        .copy(title = Some("title"), what = Some("what"),
          when = Some("when"), where = Some("where"), how = Some("how"), who = Some("who"), why = Some("why"),
          attachments = Seq("https://scontent.fybz2-2.fna.fbcdn.net/v/t34" +
            ".0-0/p480x480/21100881_10212818123624243_825936776_n.jpg?oh=cf752259cc3f8bd4adf665a41a1e97c3&oe=59A4BACE"))
      DonationUtils.publishDonation(donation, ap)
    }

    "with only some fields filled out" in {
      val donation = StateManager.createEmptyDonation(by = node)
        .copy(title = Some("title"), what = Some("what"),
          attachments = Seq("https://scontent.fybz2-2.fna.fbcdn.net/v/t34" +
            ".0-0/p480x480/21100881_10212818123624243_825936776_n.jpg?oh=cf752259cc3f8bd4adf665a41a1e97c3&oe=59A4BACE",
            "https://www.smashingmagazine.com/wp-content/uploads/2015/06/10-dithering-opt.jpg"
          ))
      DonationUtils.publishDonation(donation, ap)
    }
  }

}
