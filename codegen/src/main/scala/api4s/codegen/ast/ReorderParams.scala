package api4s.codegen.ast

import scala.collection.immutable.ListMap
import api4s.codegen.Utils._

object ReorderParams {
  private def reorderTypeParams(ts: ListMap[String, Type]): ListMap[String, Type] =
    ts.mapValueList {
      case Type.TObj(fields) =>
        val (required, optional) = fields.partition {
          case (_, Type.Field(Type.TArr(_), _, _)) => false
          case (_, f) => f.required
        }
        Type.TObj(required ++ optional)
      case t => t
    }

  private def reorderEndpointParams(e: Endpoint): Endpoint = e.copy(
    parameters = {
      val (required, optional) = e.parameters.partition(_._2.required)
      required ++ optional
    }
  )

  def apply(api: Api): Api = Api(
    types = reorderTypeParams(api.types),
    endpoints = api.endpoints //api.endpoints.mapValueList(_.mapValueList(reorderEndpointParams))
  )
}
