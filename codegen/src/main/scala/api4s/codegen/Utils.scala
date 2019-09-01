package api4s.codegen

import scala.collection.immutable.{ ListMap, SortedMap }

object Utils {
  implicit class ListMapOps[K, V](val x: ListMap[K, V]) extends AnyVal {
    def mapOnValues[U](f: V => U): ListMap[K, U] = x.map { case (k, v) => k -> f(v) }
  }

  implicit class SortedMapOps[K, V](val x: SortedMap[K, V]) extends AnyVal {
    def mapOnValues[U](f: V => U): SortedMap[K, U] = {
      implicit val ordering: Ordering[K] = x.ordering
      x.map { case (k, v) => k -> f(v) }
    }
  }
}
