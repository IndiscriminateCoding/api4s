package api4s.codegen.ast

import org.http4s.MediaRange

import scala.collection.immutable.{ ListMap, SortedMap }

case class Api(
  types: ListMap[String, Type],
  endpoints: ListMap[List[Segment], ListMap[Method, Endpoint]]
)

sealed trait Type
object Type {
  case class Field(t: Type, rn: String, required: Boolean)

  case class TMap(t: Type) extends Type
  case class TRef(name: String) extends Type
  case class TObj(fields: ListMap[String, Field]) extends Type
  case class TArr(items: Type) extends Type
  case class TJson() extends Type
  case class TInt() extends Type
  case class TLong() extends Type
  case class TNum() extends Type
  case class TString() extends Type
  case class TBool() extends Type
  case class TBinary() extends Type
}

sealed trait Method
object Method {
  case object Get extends Method
  case object Post extends Method
  case object Put extends Method
  case object Delete extends Method
  case object Options extends Method
  case object Head extends Method
  case object Patch extends Method
}

sealed trait Segment
object Segment {
  case class Static(value: String) extends Segment
  case class Argument(name: String) extends Segment
}

sealed trait ParameterType
object ParameterType {
  case object Hdr extends ParameterType
  case object Query extends ParameterType
  case object Path extends ParameterType
}

case class Endpoint(
  name: Option[String],
  requestBody: RequestBody,
  parameters: List[(ParameterType, Parameter)],
  responses: SortedMap[Option[Int], ListMap[MediaRange, Response]]
) {
  responses.getOrElse(Some(204), ListMap.empty) foreach {
    case (_, Response(Some(_), _)) =>
      throw new IllegalArgumentException(s"Non-empty 204 response for endpoint $name")
    case _ =>
  }
}

case class RequestBody(
  name: Option[String],
  ranges: ListMap[MediaRange, Type],
  required: Boolean
)

case class Response(t: Option[Type], haveHeaders: Boolean)

case class Parameter(name: String, realName: String, t: Type, required: Boolean)
