package api4s.codegen.ast

import api4s.codegen.Utils._
import api4s.codegen.ast.Segment._
import api4s.codegen.ast.Type._

import scala.collection.immutable.ListMap

object FixIdents {
  private class IDFactory(pref: String) {
    private[this] var idx = -1
    private[this] var replaced = Map.empty[String, String]

    private[this] def parts(s: String): String =
      s.map {
        case c if !c.isLetter => '-'
        case c => c
      }.split('-').filter(_.nonEmpty).map("_" + _).mkString

    def apply(s: String): String = {
      idx += 1
      val res = s"$pref$idx${parts(s)}"
      replaced = replaced.updated(s, res)
      res
    }

    def replacement(s: String): String = replaced.getOrElse(s, s)

    private[this] val re = s"$pref[0-9]+(.*)".r
    def allowed(s: String): Boolean = re.unapplySeq(s).isEmpty
  }

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
    "Map", "String", "Int", "Long", "Double", "Boolean", "Stream", "Byte", "Some", "Option",
    "List", "Throwable",

    "Json", "Encoder", "Decoder", "Request", "Response", "Status", "Sync", "CNil",  "Resource",
    "Coproduct", "UnexpectedStatus", "Method", "Applicative", "EntityEncoder", "EntityDecoder",
    "Inl", "Inr",

    "F", "S", "RoutingErrorAlgebra", "Helpers", "RichRequest", "RichUrlForm", "Endpoint",

    "Model", "Http4sServer", "Http4sClient", "Client", "Api",

    "Ok", "Created", "Accepted", "NoContent"
    //"BadRequest", "Unauthorized", "Forbidden", "NotFound",
    //"InternalServerError", "BadGateway", "ServiceUnavailable", "GatewayTimeout"
  )

  private def allowedLower(s: String): Boolean =
    s.nonEmpty &&
      s.charAt(0).isLower &&
      s.forall(c => c.isLetterOrDigit || c == '_') &&
      !keywords(s) &&
      !reservedIdentifiers(s)

  private def allowedUpper(s: String): Boolean =
    s.nonEmpty &&
      s.charAt(0).isUpper &&
      s.forall(c => c.isLetterOrDigit || c == '_') &&
      !importedSymbols(s)

  def apply(api: Api): Api = {
    val tnames = new IDFactory("T")

    def patchType(t: Type): Type = t match {
      case TMap(t) => TMap(patchType(t))
      case TRef(name) => TRef(tnames.replacement(name))
      case TObj(fields) => TObj(fields.mapValueList(f => f.copy(t = patchType(f.t))))
      case TArr(t) => TArr(patchType(t))
      case t => t
    }

    val types = api.types map {
      case (n, t) if allowedUpper(n) && tnames.allowed(n) => n -> t
      case (n, t) => tnames(n) -> t
    } mapValueList {
      case TObj(fields) =>
        TObj(fields.map {
          case (n, f) if allowedLower(n) => n -> f
          case (n, f) if n.contains("`") =>
            throw new IllegalArgumentException(s"impossible to handle field with a back-tick: $n")
          case (n, f) => s"`$n`" -> f
        })
      case t => t
    } mapValueList { t => patchType(t) }
    val opNames = new IDFactory("operation")
    val endpoints = api.endpoints map { case (segments, methods) =>
      val snames = new IDFactory("path")
      val nsegments = segments.map {
        case Argument(n) if !allowedLower(n) || !snames.allowed(n) => Argument(snames(n))
        case s => s
      }
      val nmethods = methods mapValueList { ep =>
        val paramNames = new IDFactory("param")
        Endpoint(
          name =
            if (ep.name.exists(n => allowedLower(n) && opNames.allowed(n))) ep.name
            else Some(opNames(ep.name.getOrElse("-"))),
          requestBody = RequestBody(
            name = ep.requestBody.name,
            ranges = ep.requestBody.ranges.mapValueList {
              case TObj(flds) => TObj(ListMap(flds.toList.map {
                case (k, v) if !allowedLower(k) || !paramNames.allowed(k) => paramNames(k) -> v
                case fld => fld
              }: _*))
              case t => t
            },
            required = ep.requestBody.required
          ),
          parameters = ep.parameters.map {
            case (ParameterType.Path, p) => ParameterType.Path -> p.copy(
              name = snames.replacement(p.name),
              t = patchType(p.t)
            )
            case ps @ (_, p) if allowedLower(p.name) && paramNames.allowed(p.name) => ps
            case (pt, p) => pt -> p.copy(name = paramNames(p.name))
          },
          responses = ep.responses.mapValues(_.mapValueList(r => r.copy(t = r.t.map(patchType))))
        )
      }
      nsegments -> nmethods
    }
    Api(types, endpoints)
  }
}
