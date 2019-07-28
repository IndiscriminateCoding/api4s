package api4s.codegen

import scala.collection.immutable.ListMap

object Utils {
  implicit class ListMapOps[K, V](val x: ListMap[K, V]) extends AnyVal {
    def mapValueList[U](f: V => U): ListMap[K, U] = x.map { case (k, v) => k -> f(v) }
  }
}
