package api4s.codegen.ast.idents

import api4s.codegen.Utils._
import api4s.codegen.ast.Segment._
import api4s.codegen.ast.Type._
import api4s.codegen.ast._
import api4s.codegen.ast.idents.Reserved._

import scala.collection.immutable.ListMap

object FixIdents {
  def apply(api: Api): Api = {
    val tnames = new Renamer("Type", false)

    def patchType(t: Type): Type = t match {
      case TMap(t) => TMap(patchType(t))
      case TRef(name) => TRef(tnames.find(name))
      case TObj(fields) => TObj(fields.mapOnValues(f => f.copy(t = patchType(f.t))))
      case TArr(t) => TArr(patchType(t))
      case t => t
    }

    val types = api.types map {
      case (n, t) => tnames.fix(n) -> t
    } mapOnValues {
      case TObj(fields) =>
        TObj(fields.map {
          case (n, f) if allowedField(n) => n -> f
          case (n, f) if n.contains("`") =>
            throw new IllegalArgumentException(s"impossible to handle field with a back-tick: $n")
          case (n, f) => s"`$n`" -> f
        })
      case t => t
    } mapOnValues { t => patchType(t) }
    val opNames = new Renamer("operation", true)
    val endpoints = api.endpoints map { case (segments, methods) =>
      val snames = new Renamer("path", true)
      def nsimple(s: Simple): Simple = s match {
        case Argument(n) => Argument(snames.fix(n))
        case s => s
      }
      val nsegments = segments.map {
        case s: Simple => nsimple(s)
        case Mixed(ps) => Mixed(ps.map(nsimple))
      }
      val nmethods = methods mapOnValues { ep =>
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
            ranges = ep.requestBody.ranges.mapOnValues {
              case TObj(flds) => TObj(ListMap(flds.toList.map {
                case (k, v) => paramNames.fix(k) -> v
              }: _*))
              case t => t
            },
            name = ep.requestBody.name.orElse(ep.requestBody.proposedName).map(paramNames.fix),
            required = ep.requestBody.required
          ),
          responses = ep.responses.mapOnValues(_.mapOnValues(r => r.copy(t = r.t.map(patchType))))
        )
      }
      nsegments -> nmethods
    }
    Api(types, endpoints)
  }
}
