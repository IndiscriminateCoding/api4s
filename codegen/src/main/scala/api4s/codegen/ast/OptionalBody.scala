package api4s.codegen.ast

import api4s.codegen.Utils._
import api4s.codegen.ast.ParameterType.Body

object OptionalBody {
  def apply(api: Api): Api = api.copy(
    endpoints = api.endpoints.mapValueList(_.mapValueList(e => e.copy(
      parameters = e.parameters.map {
        case (Body, Parameter(n, rn, t, false)) =>
          Body -> Parameter(n, rn, Type.TString(), true)
        case p => p
      }
    )))
  )
}
