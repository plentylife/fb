package plenty.state.model

import java.util.Date

import utest._
import boopickle.Default._

/**
  * Testing the ability to save data through pickling
  */
object Pickling extends TestSuite {
  val tests = this {
    "pickling a node" - {
      val node = Node("test_id")
      val pickled = Pickle.intoBytes(node)
      val unpickled = Unpickle[Node].fromBytes(pickled)
      assert(node.id == unpickled.id)
    }

    "pickling a coin" - {
      val node = Node("test_id")
      val coin = Coin("id", node, mintTime = new Date().getTime, deathTime = new Date().getTime, approvedBy = Set(),
        wrapsAround = None)
      val pickled = Pickle.intoBytes(coin)
      val unpickled = Unpickle[Coin].fromBytes(pickled)
      assert(coin.id == unpickled.id)
    }

    "pickling a state" - {
      val state = new State
      val pickled = Pickle.intoBytes(state)
      val unpickled = Unpickle[State].fromBytes(pickled)

      assert(state.nodes == unpickled.nodes)
    }
  }
}
