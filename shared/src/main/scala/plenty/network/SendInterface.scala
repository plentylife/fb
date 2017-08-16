package plenty.network

/**
  * [[plenty.network.Network]] uses implementation of these interfaces to send out messages into the world
  * */

trait SendInterface {
  def send(msg: Message[_]): Unit
}