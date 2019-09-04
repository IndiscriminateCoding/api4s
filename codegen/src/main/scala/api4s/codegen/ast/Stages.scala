package api4s.codegen.ast

import api4s.codegen.ast.idents.FixIdents

object Stages {
  def apply(api: Api): Api = ReorderFields(FixIdents(api))
}
