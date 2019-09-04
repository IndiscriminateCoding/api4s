package api4s.codegen.ast.idents

import api4s.codegen.ast.idents.Reserved._

class Renamer(
  prefix: String,
  lowercase: Boolean,
  ctx: List[(String, String)] = Nil
) {
  private[this] var replacements = ctx

  def currentCtx: List[(String, String)] = replacements

  def allowed(s: String): Boolean = s.nonEmpty &&
    !keywords(s) &&
    !reservedIdentifiers(s) &&
    !importedSymbols(s) &&
    s.forall(letterDigitOrUnderscore) &&
    (if (lowercase) s.charAt(0).isLower else s.charAt(0).isUpper)

  def uppercased(s: String): String = s.capitalize

  def lowercased(s: String): String = {
    val chars = s.toCharArray
    chars(0) = chars(0).toLower
    new String(chars)
  }

  def parts(s: String): String =
    s.map {
      case c if !letterDigitOrUnderscore(c) => '-'
      case c => c
    }.split('-').filter(_.nonEmpty).map(uppercased).mkString

  def fix(s: String): String = {
    val res = {
      val ps = {
        val res = parts(s)
        if (res.nonEmpty && res.charAt(0).isLetter) res
        else prefix + res
      }
      val str = if (lowercase) lowercased(ps) else uppercased(ps)

      @scala.annotation.tailrec
      def findFree(s: String, idx: Long): String = {
        val si = if (idx < 0) s else s"$s$idx"
        if (replacements.exists(_._2 == si) || !allowed(si)) findFree(s, idx + 1)
        else si
      }

      val re = raw"(\d{0,8})(.*)".r
      str.reverse match {
        case re("", _) => findFree(str, -1)
        case re(idx, x) => findFree(x.reverse, idx.reverse.toLong)
        case _ => throw new Exception("never happens")
      }
    }

    replacements = s -> res :: replacements
    res
  }

  def find(s: String): String = replacements
    .find(_._1 == s)
    .getOrElse(throw new Exception(s"[$prefix] Identifier '$s' replacement not found"))
    ._2
}
