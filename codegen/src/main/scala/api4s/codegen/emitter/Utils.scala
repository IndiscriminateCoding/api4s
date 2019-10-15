package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast.{ Produces, Type }
import api4s.codegen.utils.Registry.registry

object Utils {
  object default extends Utils()
}

class Utils(F: String = "F", S: String = "S") {
  def producesPlain(p: Produces): String = p match {
    case Produces.Untyped => s"Resource[$F, Response[$S]]"
    case Produces.One(_, content) => typeStr(content.map(_._2))
    case Produces.Many(rs) => s"${
      rs.map {
        case (s, _) if registry.values.exists { case (n, r) => !r && n == s } => s
        case (s, t) => s"$s[${typeStr(t.map(_._2))}]"
      }.mkString(" :+: ")
    } :+: CNil"
  }

  def producesLifted(p: Produces): String = p match {
    case Produces.Untyped => producesPlain(p)
    case _ => s"$F[${producesPlain(p)}]"
  }

  def shapelessPat(i: Int, v: String): String =
    if (i == 0) s"Inl($v)"
    else s"Inr(${shapelessPat(i - 1, v)})"

  def shapelessCNil(i: Int): String =
    if (i == 1) s"Inr(cnil)"
    else s"Inr(${shapelessCNil(i - 1)})"

  def typeStr(t: Type): String = t match {
    case TRef(name) => name
    case TMap(et) => s"Map[String, ${typeStr(et)}]"
    case TArr(it) => s"List[${typeStr(it)}]"
    case TJson => "Json"
    case TInt => "Int"
    case TLong => "Long"
    case TFloat => "Float"
    case TDouble => "Double"
    case TString => "String"
    case TBool => "Boolean"
    case TMedia => s"Media[$S]"
    case TObj(flds) =>
      val types = flds.values.map(_.t).toSet
      val fldType =
        if (types.size == 1) typeStr(types.head)
        else "Json"
      s"Map[String, $fldType]"
  }

  val primitive: Set[Type] = Set(TString, TInt, TLong, TFloat, TDouble, TBool)

  def typeStr(t: Option[Type]): String = t.fold("Unit")(typeStr)
}
