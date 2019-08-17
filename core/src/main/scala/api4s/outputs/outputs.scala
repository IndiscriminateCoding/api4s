package api4s.outputs

case class Ok[A](content: A)
object Ok {
  def apply(): Ok[Unit] = Ok(())
}

case class Created[A](content: A)
object Created {
  def apply(): Created[Unit] = Created(())
}

case class Accepted[A](content: A)
object Accepted {
  def apply(): Accepted[Unit] = Accepted(())
}

case class NoContent()
