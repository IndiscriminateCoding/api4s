package api4s.codegen.ast

import org.http4s.MediaRange

import scala.collection.immutable.ListMap

sealed trait Produces

object Produces {
  case object Untyped extends Produces
  case class One(status: String, content: Option[(MediaRange, Type)]) extends Produces
  case class Many(rs: ListMap[String, Option[(MediaRange, Type)]]) extends Produces
}
