package fb.donation

import org.scalatest.FreeSpec
import plenty.agent.{AgentManager, AgentPointer}
import plenty.agent.model.Agent
import plenty.state.StateManager
import plenty.state.model.{Node, State}

class DonationTests extends FreeSpec {
  // dev
//  val anton = "1783146675033183"
  // prod
  val anton = "767613720030082"
  val andrey = "1624828950901835"
  val toId = anton

  val node = Node(toId)
  val a = Agent(toId, State())
  val ap = new AgentPointer(a)

  "Publishing of donation posts on page" - {
    "with all fields filled out" in {
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

  "Donation flow notifications" - {
    "displaying donation bubble after creation" in {
      val donation = StateManager.createEmptyDonation(by = node)
        .copy(title = Some("title"), what = Some("what"),
          when = Some("when"), where = Some("where"), how = Some("how"), who = Some("who"), why = Some("why"),
          attachments = Seq("https://scontent.fybz2-2.fna.fbcdn.net/v/t34" +
            ".0-0/p480x480/21100881_10212818123624243_825936776_n.jpg?oh=cf752259cc3f8bd4adf665a41a1e97c3&oe=59A4BACE"))
      DonationResponses.showDonationBubble(ap, donation, Some("fake"))
    }

    "displaying donation bubble after creation with long subtitle" in {
      val donation = StateManager.createEmptyDonation(by = node)
        .copy(title = Some("title"), what = Some("such very long descr aston oeast ewoten etukvkwu kkoiwti ekraiekt " +
          "iekst uwtk ki ekarosk uvkwiote kvoiakv ywk yukvik raikv yuwkv ikraivk kvwuykvioarkv krk ywukvoikvirakv " +
          "wukykvirkvk ruywkvikaivkywukviakvkaryukwivkrievk kywvkwyugwakvwkyukwiekvir kwaykvoiwk ivkywuk iowkvoi"))
      DonationResponses.showDonationBubble(ap, donation, Some("fake"))
    }

    "displaying donation bubble in bid mode" in {
      val donation = StateManager.createEmptyDonation(by = node)
        .copy(title = Some("title"), what = Some("what"),
          when = Some("when"), where = Some("where"), how = Some("how"), who = Some("who"), why = Some("why"),
          attachments = Seq("https://scontent.fybz2-2.fna.fbcdn.net/v/t34" +
            ".0-0/p480x480/21100881_10212818123624243_825936776_n.jpg?oh=cf752259cc3f8bd4adf665a41a1e97c3&oe=59A4BACE"))
      DonationResponses.showDonationBubble(ap, donation, None, biddingMode = true)
    }
  }

  "Posting" - {
    "should not time out" in {
      val fakeNode = Node(anton)
      val fakeAgent = Agent(fakeNode.id, State())
      val fakePointer = new AgentPointer(fakeAgent)
      val fakeDonation = StateManager.createEmptyDonation(fakeNode)
      DonationUtils.publishDonation(fakeDonation, fakePointer)
    }

    val realPostId = "1420741541308144_1438598576189107"
    "should be able to update with action links" in {
      val fakeNode = Node(anton)
      val fakeDonation = StateManager.createEmptyDonation(fakeNode).copy(id = realPostId)
      DonationUtils.finalizeDonationPost(fakeDonation)
    }

    "should be able to mark as settled" in {
      val fakeNode = Node(anton)
      val fakeDonation = StateManager.createEmptyDonation(fakeNode).copy(id = realPostId)
      val fakeBid = StateManager.createBid(donation = fakeDonation, amount = 7, by=fakeNode)
      ExternalDonationUtils.markPostAsSettled(fakeBid)
    }
  }

}
