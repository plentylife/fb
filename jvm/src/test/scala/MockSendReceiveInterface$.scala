import plenty.network.{Message, Network, SendReceiveInterface}

object MockSendReceiveInterface extends SendReceiveInterface {
  override def send(msg: Message[_]): Unit = Network.receive(msg)
}