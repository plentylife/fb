package fb

import fb.donation.DonationResponses
import org.scalatest.{FreeSpec, Matchers}
import plenty.agent.model.Agent
import plenty.network.Network
import plenty.state.StateManager
import plenty.state.model.{Donation, Node}

import scala.concurrent.Promise

class MiscFbTests extends FreeSpec with Matchers {

  "Google shortner service" in {
    val link = Utility.getShortLink("plenty.life")
    println(link)

    "should produce a valid link" in {
      assert(link.nonEmpty)
    }
  }

  "Email" - {
    "should be sent" in {
      Utility.sendEmail(EmailInfo("subject", "this is a test problem", "antonkats@gmail.com")) shouldBe 0
    }
  }

  "Report a problem button should be present beside leaving a comment" in {
    // anton in test
    val anton = "1783146675033183"
    DonationResponses.askToLeaveContact(UserInfo(anton, "anton", "last name"),
      Donation(id = "fake", by = Node(anton), timestamp = 0), explanation = "explanation")
  }

  "Clear bids" in {
    import com.softwaremill.quicklens._
    StateManager.loadAll() foreach { a => Network.registerAgent(a, FbSendReceiveInterface) }
    Network.getAgents.foreach {ap ⇒
        val upd = ap.agentInLastState.modify(_.state.bids).using(_ ⇒ Set())
        StateManager.save(upd)
    }
  }
}
