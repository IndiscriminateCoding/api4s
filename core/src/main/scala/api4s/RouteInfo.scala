package api4s

import cats.effect.SyncIO
import org.typelevel.vault.Key

case class RouteInfo(version: String, operationId: String)

object RouteInfo {
  val key: Key[RouteInfo] = Key.newKey[SyncIO, RouteInfo].unsafeRunSync()
}
