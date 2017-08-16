import plenty.network.{Message, Network, SendInterface}

object MockSendInterface extends SendInterface {
  override def send(msg: Message[_]): Unit = Network.receive(msg)
}