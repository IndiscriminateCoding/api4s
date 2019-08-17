package api4s

import org.http4s._

case class Media[F[_]](
  body: fs2.Stream[F, Byte] = fs2.Stream.empty,
  mediaType: Option[MediaType] = None,
  length: Option[Long] = None
) {
  require(length.forall(_ >= 0), "Content-Length should be >= 0!")

  def headers: Headers = {
    val hdrs = scala.collection.mutable.Buffer[Header]()
    mediaType.foreach(mt => hdrs += org.http4s.headers.`Content-Type`(mt))
    length.foreach(l => hdrs += Header("Content-Length", l.toString))
    Headers(hdrs.toList)
  }
}

object Media {
  def apply[F[_]](msg: Message[F]): Media[F] = Media(
    body = msg.body,
    mediaType = headers.`Content-Type`.from(msg.headers).map(_.mediaType),
    length = headers.`Content-Length`.from(msg.headers).map(_.length)
  )
}
