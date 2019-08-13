package api4s.codegen.ast

import api4s.codegen.ast.Type.Field
import org.http4s.MediaType

sealed trait Consumes

object Consumes {
  case object Empty extends Consumes
  case class FormData(flds: List[(String, Field)]) extends Consumes
  case class JsonBody(name: String, t: Type) extends Consumes
  case class Entity(name: String, mt: Option[MediaType]) extends Consumes
}
