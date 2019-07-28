package api4s.codegen.ast

import api4s.codegen.Utils._

object CollapsedMultipart {
  def apply(api: Api): Api = api.copy(
    endpoints = api.endpoints.mapValueList(_.mapValueList(e => e.copy(
      parameters = {
        val filtered = e.parameters.filter(_._1 != ParameterType.FormData)
        val collapsed =
          if (filtered.size != e.parameters.size) List(
            ParameterType.FormData -> Parameter("formData", "formData", Type.TFile(), true)
          ) else Nil
        filtered ++ collapsed
      }
    )))
  )
}
