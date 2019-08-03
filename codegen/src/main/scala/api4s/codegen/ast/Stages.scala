package api4s.codegen.ast

object Stages {
  def apply(api: Api): Api = ReorderParams(FixIdents(api))
}
