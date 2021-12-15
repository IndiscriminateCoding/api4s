package api4s.codegen.ast

import scala.collection.immutable.ListMap
import api4s.codegen.Utils._

object ReorderFields {
  private def reorderTypeParams(ts: ListMap[String, Type]): ListMap[String, Type] =
    ts.mapOnValues {
      case Type.TObj(fields) =>
        val (required, optional) = fields.partition {
          case (_, Type.Field(Type.TArr(_), _, _)) => false
          case (_, f) => f.required
        }
        Type.TObj(required ++ optional)
      case t => t
    }

  def apply(api: Api): Api = Api(
    version = api.version,
    types = reorderTypeParams(api.types),
    endpoints = api.endpoints
  )
}
