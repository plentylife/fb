package plenty.state.model

trait HasId[T] {
  val id: T
}

trait EquatableById[IdT] extends HasId[IdT] {
  override def equals(o: scala.Any): Boolean = o match {
    case o: HasId[IdT] => o.id == this.id
    case _ => false
  }

  override def hashCode(): Int = this.id.hashCode()
}
