package plenty.network

/**
  * [[plenty.network.Network]] uses implementation of these interfaces to send out messages into the world
  * */

trait SendReceiveInterface {
  def send(msg: Message[_]): Unit
}