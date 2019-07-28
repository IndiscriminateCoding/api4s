package api4s.runtime.outputs

import org.http4s.{ EntityEncoder, Response, Status }
import shapeless.Poly1

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

trait ToResponse[F[_]] extends Poly1 {
  implicit def ok[A](implicit E: EntityEncoder[F, A]): Case.Aux[Ok[A], Response[F]] =
    at[Ok[A]] { x =>
      Response(
        status = Status.Ok,
        body = E.toEntity(x.content).body,
        headers = E.headers
      )
    }
  implicit def created[A](implicit E: EntityEncoder[F, A]): Case.Aux[Created[A], Response[F]] =
    at[Created[A]] { x =>
      Response(
        status = Status.Created,
        body = E.toEntity(x.content).body,
        headers = E.headers
      )
    }
  implicit def accepted[A](implicit E: EntityEncoder[F, A]): Case.Aux[Accepted[A], Response[F]] =
    at[Accepted[A]] { x =>
      Response(
        status = Status.Accepted,
        body = E.toEntity(x.content).body,
        headers = E.headers)
    }
  implicit def noContent: Case.Aux[NoContent, Response[F]] =
    at[NoContent] { _ => Response(status = Status.NoContent) }
}
