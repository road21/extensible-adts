package adts

import scala.quoted.*
import ADT.~

//-----------------------------------------------

class AND[l, r]
class OR[l, r]

//-----------------------------------------------

trait FieldsTree[R]:
  type Tree
  type CommonNames <: Tuple
  type CommonValues <: Tuple

object FieldsTree extends FieldsTupleDerivation:
  type Aux[R, T, CN <: Tuple, CV <: Tuple] = FieldsTree[R]:
    type Tree = T
    type CommonNames = CN
    type CommonValues = CV

  type Of[R, T] = FieldsTree[R]:
    type Tree = T

trait FieldsTupleDerivation:
  transparent inline given derive[A]: FieldsTree[A] =
    ${ FieldsTupleDerivation.inst[A] }

trait TypTree[q <: Quotes]
object TypTree:
  case class And[q <: Quotes](l: TypTree[q], r: TypTree[q]) extends TypTree[q]
  case class Or[q <: Quotes](l: TypTree[q], r: TypTree[q]) extends TypTree[q]

  trait Typ[q <: Quotes](using val q: q) extends TypTree[q]:
    def name: String
    def nameRepr: q.reflect.TypeRepr
    def typRepr: q.reflect.TypeRepr
    def fullRepr: q.reflect.TypeRepr

object FieldsTupleDerivation:
  def inst[R: Type](using
      q: Quotes
  ): Expr[FieldsTree[R]] =
    import quotes.reflect.*

    def decompose(base: TypeRepr): TypTree[q.type] =
      base.dealias match
        case AndType(l, r) =>
          TypTree.And(decompose(l), decompose(r))
        case OrType(l, r) =>
          TypTree.Or(decompose(l), decompose(r))
        case AppliedType(constr, List(n, v)) if (constr =:= TypeRepr.of[~]) =>
          n.asType match
            case '[type s <: String; s] =>
              Type.valueOfConstant[s] match
                case None => report.errorAndAbort(s"unexpected")
                case Some(value) =>
                  new TypTree.Typ[q.type]:
                    val fullRepr = base
                    val name = value
                    val nameRepr = n
                    val typRepr = v
        case _ =>
            report.errorAndAbort(s"Unable to decompose param for ${base.show}")

    def toTyp(typs: TypTree[q.type]): TypeRepr =
      typs match
        case TypTree.And(l, r) => TypeRepr.of[AND].appliedTo(List(toTyp(l), toTyp(r)))
        case TypTree.Or(l, r) => TypeRepr.of[OR].appliedTo(List(toTyp(l), toTyp(r)))
        case t: TypTree.Typ[q.type] => t.fullRepr

    def common(typs: TypTree[q.type]): Vector[(String, TypeRepr)] =
      typs match
        case TypTree.And(l, r) =>
            common(l) ++ common(r)
        case TypTree.Or(l, r) =>
            val m = common(l).map(x => x._1 -> x._2).toMap
            common(r).map(x => x -> m.get(x._1)).collect {
              case ((n, repr), Some(v)) =>
                (n, TypeRepr.of[[x, y] =>> x | y].appliedTo(List(repr, v)))
            }
        case t: TypTree.Typ[q.type] =>
            Vector(t.name -> t.typRepr)

    def tupled(typs: Vector[(String, TypeRepr)]): (TypeRepr, TypeRepr) =
      typs match
        case (n, v) +: t =>
          val (names, values) = tupled(t)
          (
            TypeRepr.of[*:].appliedTo(List(ConstantType(StringConstant(n)), names)),
            TypeRepr.of[*:].appliedTo(List(v, values))
          )
        case _ => (TypeRepr.of[EmptyTuple], TypeRepr.of[EmptyTuple])

    val decomposed = decompose(TypeRepr.of[R].dealias.simplified)
    val (names, values) = tupled(common(decomposed))
    (toTyp(decomposed).asType, names.asType, values.asType) match
      case ('[x], '[type cn <: Tuple; cn], '[type cv <: Tuple; cv]) =>
        '{
          (new FieldsTree[R] {
            type Tree = x
            type CommonNames = cn
            type CommonValues = cv
          }): FieldsTree.Aux[R, x, cn, cv]
        }
