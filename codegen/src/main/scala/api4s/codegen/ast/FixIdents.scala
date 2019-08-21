package api4s.codegen.ast

import api4s.codegen.Utils._
import api4s.codegen.ast.Segment._
import api4s.codegen.ast.Type._

import scala.collection.immutable.ListMap

object FixIdents {
  private val keywords = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
    "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null",
    "object", "override", "package", "private", "protected", "return", "sealed", "super", "this",
    "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield"
  )
  private val reservedIdentifiers = Set(
    "mutable", "request", "formData", "api", "client", "http4s"
  )
  private val importedSymbols = Set(
    "Map", "String", "Int", "Long", "Double", "Boolean", "Byte", "Some", "Option", "List",
    "Throwable",

    "Json", "Encoder", "Decoder", "Request", "Response", "Status", "Sync", "CNil", "Resource",
    "Coproduct", "UnexpectedStatus", "Method", "Applicative", "EntityEncoder", "EntityDecoder",
    "Inl", "Inr",

    "F", "S", "RoutingErrorAlgebra", "Helpers", "RichRequest", "RichUrlForm", "Endpoint", "Decode",
    "Errors",

    "Model", "Http4sServer", "Http4sClient", "Client", "Api", "Media",

    "Ok", "Created", "Accepted", "NoContent"
    //"BadRequest", "Unauthorized", "Forbidden", "NotFound",
    //"InternalServerError", "BadGateway", "ServiceUnavailable", "GatewayTimeout"
  )

  private class Renamer(
    prefix: String,
    lowercase: Boolean,
    ctx: Map[String, String] = Map.empty
  ) {
    private[this] var replacements = ctx

    def currentCtx: Map[String, String] = replacements

    def allowed(s: String): Boolean = s.nonEmpty &&
      !keywords(s) &&
      !reservedIdentifiers(s) &&
      !importedSymbols(s) &&
      s.forall(_.isLetterOrDigit) &&
      (if (lowercase) s.charAt(0).isLower else s.charAt(0).isUpper)

    def uppercased(s: String): String = s.capitalize

    def lowercased(s: String): String = {
      val chars = s.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    }

    def parts(s: String): String =
      s.map {
        case c if !c.isLetter && !c.isDigit => '-'
        case c => c
      }.split('-').filter(_.nonEmpty).map(uppercased).mkString

    def fix(s: String): String = {
      val res =
        if (allowed(s) && !replacements.exists(_._2 == s)) s
        else {
          val ps = {
            val res = parts(s)
            if (res.nonEmpty && !res.charAt(0).isDigit) res
            else prefix + res
          }
          val str = if (lowercase) lowercased(ps) else uppercased(s)

          @scala.annotation.tailrec
          def findFree(s: String, idx: Long): String = {
            val si = s"$s$idx"
            if (replacements.exists(_._2 == si)) findFree(s, idx + 1)
            else si
          }

          val re = raw"(\d{0,8})(.*)".r
          str.reverse match {
            case _ if allowed(str) && !replacements.exists(_._2 == str) => str
            case re("", _) => findFree(str, 0)
            case re(idx, s) => findFree(s.reverse, idx.reverse.toLong)
            case _ => throw new Exception("never happens")
          }
        }
      replacements = replacements.updated(s, res)
      res
    }

    def find(s: String): String = replacements.getOrElse(
      s,
      throw new Exception(s"[$prefix] Identifier '$s' replacement not found")
    )
  }

  private def allowedField(s: String): Boolean =
    s.nonEmpty &&
      s.forall(c => c.isLetterOrDigit || c == '_') &&
      !keywords(s)

  def apply(api: Api): Api = {
    val tnames = new Renamer("Type", false)

    def patchType(t: Type): Type = t match {
      case TMap(t) => TMap(patchType(t))
      case TRef(name) => TRef(tnames.find(name))
      case TObj(fields) => TObj(fields.mapValueList(f => f.copy(t = patchType(f.t))))
      case TArr(t) => TArr(patchType(t))
      case t => t
    }

    val types = api.types map {
      case (n, t) => tnames.fix(n) -> t
    } mapValueList {
      case TObj(fields) =>
        TObj(fields.map {
          case (n, f) if allowedField(n) => n -> f
          case (n, f) if n.contains("`") =>
            throw new IllegalArgumentException(s"impossible to handle field with a back-tick: $n")
          case (n, f) => s"`$n`" -> f
        })
      case t => t
    } mapValueList { t => patchType(t) }
    val opNames = new Renamer("operation", true)
    val endpoints = api.endpoints map { case (segments, methods) =>
      val snames = new Renamer("path", true)
      val nsegments = segments.map {
        case Argument(n) => Argument(snames.fix(n))
        case s => s
      }
      val nmethods = methods mapValueList { ep =>
        val paramNames = new Renamer("param", true, snames.currentCtx)
        Endpoint(
          name = Some(opNames.fix(ep.name.getOrElse(""))),
          parameters = ep.parameters.map {
            case (Parameter.Path, p) => Parameter.Path -> p.copy(
              name = snames.find(p.name),
              t = patchType(p.t)
            )
            case (pt, p) => pt -> p.copy(name = paramNames.fix(p.name))
          },
          requestBody = RequestBody(
            ranges = ep.requestBody.ranges.mapValueList {
              case TObj(flds) => TObj(ListMap(flds.toList.map {
                case (k, v) => paramNames.fix(k) -> v
              }: _*))
              case t => t
            },
            name = ep.requestBody.name.orElse(ep.requestBody.proposedName).map(paramNames.fix),
            required = ep.requestBody.required
          ),
          responses = ep.responses.mapValues(_.mapValueList(r => r.copy(t = r.t.map(patchType))))
        )
      }
      nsegments -> nmethods
    }
    Api(types, endpoints)
  }
}
