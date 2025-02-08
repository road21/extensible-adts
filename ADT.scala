package adts

import scala.util.NotGiven

import scala.annotation.implicitNotFound

import scala.language.experimental.namedTuples
import scala.Tuple.{Contains, Concat, Zip}
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag
import ADT.*
import scala.NamedTuple.NamedTuple
import scala.compiletime.{summonInline, erasedValue, constValue}
import scala.language.dynamics
import kyo.SafeClassTag

type Get[N <: String, T] = T match
  case (N, v) *: t => v
  case (x, _) *: t => Get[N, t]

class ADT[+R] private[adts] (val toMap: Map[String, (LightTypeTag, Any)])
    extends Dynamic:

  inline def selectDynamic[Name <: String & Singleton, R1 >: R](
      name: Name
  )(using ft: FieldsTree[R1])(using
      Contains[ft.CommonNames, Name] =:= true
  ): Get[Name, Zip[ft.CommonNames, ft.CommonValues]] =
    toMap(name)._2.asInstanceOf

  def &[A](other: ADT[A]): ADT[R & A] =
    ADT(toMap ++ other.toMap)

object ADT:
  class ~[Name <: String, +Value]

  extension (self: String)
    def ~[Value <: Singleton: Tag](value: Value): ADT[self.type ~ Value] =
      new ADT[self.type ~ Value](
        Map.empty.updated(self, (Tag[Value].tag, value))
      )

    def ~~[Value: Tag](value: Value): ADT[self.type ~ Value] =
      new ADT[self.type ~ Value](
        Map.empty.updated(self, (Tag[Value].tag, value))
      )

object Like:
  private def single[n <: String, v](
      n: n,
      v: Any,
      t: LightTypeTag
  ): ADT[n ~ v] =
    new ADT[n ~ v](Map.empty.updated(n, (t, v)))

  private inline def matches[R](adt: ADT[Any]): Option[ADT[?]] =
    inline erasedValue[R] match
      case _: (n ~ v) =>
        val n = constValue[n]
        adt.toMap.get(n).flatMap { case (t, v) =>
          val tag = summonInline[Tag[v]]
          val classTag = summonInline[SafeClassTag[v]]
          Option.when(t <:< tag.tag || classTag.accepts(v))(single(n, v, t))
        }
      case _: AND[l, r] =>
        for
          lr <- matches[l](adt)
          rr <- matches[r](adt)
        yield lr & rr
      case _: OR[l, r] =>
        matches[l](adt).orElse(matches[r](adt))

  class Unapply[Target](private val dummy: Boolean = false) extends AnyVal:
    inline def unapply[Repr, A](adt: ADT[A])(using
        FieldsTree.Of[Target, Repr]
    ): Option[ADT[Target]] =
      matches[Repr](adt).asInstanceOf

  inline def apply[Target]: Unapply[Target] = new Unapply[Target]