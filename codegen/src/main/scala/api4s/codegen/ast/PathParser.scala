package api4s.codegen.ast

import api4s.codegen.ast.Segment._

object PathParser {
  def apply(p: String): List[Segment] = p.split('/').filter(_.nonEmpty).toList map {
    case s if s.forall(c => c != '{' && c != '}') => Static(s)
    case s => parseMixed.static(s.toList, Nil) match {
      case one :: Nil => one
      case many => Mixed(many)
    }
  }

  private object parseMixed {
    def static(cs: List[Char], acc: List[Char]): List[Simple] = cs match {
      case Nil if acc.isEmpty => Nil
      case Nil => List(Static(acc.reverse.mkString))
      case '{' :: t if acc.isEmpty => argument(t, Nil)
      case '{' :: t => Static(acc.reverse.mkString) :: argument(t, Nil)
      case h :: t => static(t, h :: acc)
    }

    def argument(cs: List[Char], acc: List[Char]): List[Simple] = cs match {
      case Nil => List(Argument(acc.reverse.mkString))
      case '}' :: t => Argument(acc.reverse.mkString) :: static(t, Nil)
      case h :: t => argument(t, h :: acc)
    }
  }
}
