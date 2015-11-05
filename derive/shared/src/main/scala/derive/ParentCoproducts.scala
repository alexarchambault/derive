package derive

import shapeless.{ DepFn1, HList, ops, CaseClassMacros }

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait ParentCoproduct[T] extends DepFn1[T]

object ParentCoproduct {
  def apply[T](implicit top: ParentCoproduct[T]): Aux[T, top.Out] = top

  type Aux[T, Out0] = ParentCoproduct[T] { type Out = Out0 }

  implicit def fromParentCoproducts[T, L <: HList, P, PT <: HList]
   (implicit
     parCoprod: ParentCoproducts.Aux[T, L],
     isHCons: ops.hlist.IsHCons.Aux[L, P, PT]
   ): Aux[T, P] =
    new ParentCoproduct[T] {
      type Out = P
      def apply(t: T) = parCoprod(t).head
    }
}

trait ParentCoproducts[T] extends DepFn1[T] {
  type Out <: HList
}

object ParentCoproducts {
  def apply[T](implicit memberOf: ParentCoproducts[T]): Aux[T, memberOf.Out] = memberOf

  type Aux[T, Out0 <: HList] = ParentCoproducts[T] { type Out = Out0 }

  implicit def materialize[T, Out0 <: HList]: Aux[T, Out0] = macro ParentCoproductsMacros.materialize[T, Out0]

  def instance[T, Out0 <: HList](f: T => Out0): Aux[T, Out0] =
    new ParentCoproducts[T] {
      type Out = Out0
      def apply(t: T) = f(t)
    }
}

@macrocompat.bundle
class ParentCoproductsMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def materialize[T: WeakTypeTag, Out0: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].dealias

    if (!isProduct(tpe))
      abort(s"$tpe is not case class-like")

    val parTpes = tpe
      .baseClasses
      .drop(1)
      .map(s => s.asType.toType.asSeenFrom(tpe, s))
      .filter(isCoproduct)
      .reverse

    val outTpe = mkHListTpe(parTpes)
    val outTree = parTpes.foldRight(q"_root_.shapeless.HNil": Tree) {
      case (_, acc) => q"_root_.shapeless.::(t, $acc)"
    }

    q"_root_.shapeless.ParentCoproducts.instance[$tpe, $outTpe](t => $outTree)"
  }
}
