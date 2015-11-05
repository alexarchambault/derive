package derive

import scala.language.experimental.macros
import scala.annotation.tailrec
import scala.reflect.macros.whitebox
import shapeless._

trait Derive[+T] {
  def value: T
}

object Derive extends LazyExtensionCompanion {
  def apply[T](t: T): Derive[T] =
    new Derive[T] {
      def value = t
    }

  implicit def init[T]: Derive[T] = macro initImpl

  def instantiate(ctx0: DerivationContext): LazyExtension { type Ctx = ctx0.type } =
    new DeriveLazyExtension {
      type Ctx = ctx0.type
      val ctx: ctx0.type = ctx0
    }

  /** shapeless 3.0 -like Generic? Provides generic representations as nested Tuple2 and Either
    * (respectively terminated by Unit and Nothing) */
  trait Generic[T] extends shapeless.Generic[T]

  object Generic {
    type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }

    def apply[T](implicit gen: Generic[T]): Aux[T, gen.Repr] = gen

    implicit def materialize[T, R]: Aux[T, R] = macro DeriveMacros.materializeDeriveGeneric[T, R]

    def unsafeMkSum(length: Int, value: Any) =
      (0 until length).foldLeft[Either[_, _]](Left(value))((accum, _) => Right(accum))

    @tailrec
    def unsafeGet(c: Either[_, _]): Any = c match {
      case Left(h) => h
      case Right(c) => unsafeGet(c.asInstanceOf[Either[_, _]])
    }
  }
}

@macrocompat.bundle
class DeriveMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def unitTpe = typeOf[Unit]
  def tuple2Tpe = typeOf[(_, _)].typeConstructor
  def nothingTpe = typeOf[Nothing]
  def eitherTpe = typeOf[Either[_, _]]

  def deriveReprTypTree(tpe: Type): Tree = {
    if(isProduct(tpe)) mkCompoundTypTree(unitTpe, tuple2Tpe, fieldsOf(tpe).map(_._2))
    else mkCompoundTypTree(nothingTpe, eitherTpe, ctorsOf(tpe))
  }

  def mkProductCases(tpe: Type, nil: Tree, cons: Tree): (CaseDef, CaseDef) = {
    if(tpe =:= typeOf[Unit])
      (
        cq"() => $nil",
        cq"$nil => ()"
        )
    else if(isCaseObjectLike(tpe.typeSymbol.asClass)) {
      val singleton =
        tpe match {
          case SingleType(pre, sym) =>
            c.internal.gen.mkAttributedRef(pre, sym)
          case TypeRef(pre, sym, List()) if sym.isModule =>
            c.internal.gen.mkAttributedRef(pre, sym.asModule)
          case TypeRef(pre, sym, List()) if sym.isModuleClass =>
            c.internal.gen.mkAttributedRef(pre, sym.asClass.module)
          case other =>
            abort(s"Bad case object-like type $tpe")
        }

      (
        cq"_: $tpe => $nil",
        cq"$nil => $singleton: $tpe"
        )
    } else {
      val sym = tpe.typeSymbol
      val isCaseClass = sym.asClass.isCaseClass
      def hasNonGenericCompanionMember(name: String): Boolean = {
        val mSym = sym.companion.typeSignature.member(TermName(name))
        mSym != NoSymbol && !isNonGeneric(mSym)
      }

      val binders = fieldsOf(tpe).map { case (name, tpe) => (TermName(c.freshName("pat")), name, tpe, isVararg(tpe)) }

      val to =
        if(isCaseClass || hasNonGenericCompanionMember("unapply")) {
          val wcard = Star(Ident(termNames.WILDCARD))  // like pq"_*" except that it does work
          val lhs = pq"${companionRef(tpe)}(..${binders.map(x => if (x._4) pq"${x._1} @ $wcard" else pq"${x._1}")})"
          val rhs =
            binders.foldRight(nil) {
              case ((bound, name, tpe, _), acc) =>
                tpe match {
                  case ConstantType(c) =>
                    q"$cons($c, $acc)"
                  case _ =>
                    q"$cons($bound, $acc)"
                }
            }
          cq"$lhs => $rhs"
        } else {
          val lhs = TermName(c.freshName("pat"))
          val rhs =
            fieldsOf(tpe).foldRight(nil) {
              case ((name, tpe), acc) => q"$cons($lhs.$name, $acc)"
            }
          cq"$lhs => $rhs"
        }

      val from = {
        val lhs =
          binders.foldRight(nil) {
            case ((bound, _, _, _), acc) => pq"$cons($bound, $acc)"
          }

        val rhs = {
          val ctorArgs = binders.map { case (bound, name, tpe, vararg) =>
            if (vararg) q"$bound: _*"
            else
              tpe match {
                case ConstantType(c) =>
                  q"$c.asInstanceOf[$tpe]"
                case _ =>
                  Ident(bound)
              }
          }
          if(isCaseClass || hasNonGenericCompanionMember("apply"))
            q"${companionRef(tpe)}(..$ctorArgs)"
          else
            q"new $tpe(..$ctorArgs)"
        }

        cq"$lhs => $rhs"
      }

      (to, from)
    }
  }

  def mkCoproductCases(tpe0: Type, index: Int): CaseDef = {
    tpe0 match {
      case SingleType(pre, sym) =>
        val singleton = mkAttributedRef(pre, sym)
        cq"p if p eq $singleton => $index"
      case _ =>
        cq"_: $tpe0 => $index"
    }
  }

  def mkProductDeriveGeneric(tpe: Type): Tree = {
    val (toCases, fromCases) = {
      val (to, from) = mkProductCases(tpe, q"()", q"_root_.scala.Tuple2")
      (List(to), List(from))
    }

    val clsName = TypeName(c.freshName())
    q"""
      final class $clsName extends _root_.derive.Derive.Generic[$tpe] {
        type Repr = ${deriveReprTypTree(tpe)}
        def to(p: $tpe): Repr = (p match { case ..$toCases }).asInstanceOf[Repr]
        def from(p: Repr): $tpe = p match { case ..$fromCases }
      }
      new $clsName(): _root_.derive.Derive.Generic.Aux[$tpe, ${deriveReprTypTree(tpe)}]
    """
  }

  def mkCoproductDeriveGeneric(tpe: Type): Tree = {
    val to = {
      val toCases = ctorsOf(tpe) zip (Stream from 0) map (mkCoproductCases _).tupled
      q"""_root_.derive.Derive.Generic.unsafeMkSum((p: @_root_.scala.unchecked) match { case ..$toCases }, p).asInstanceOf[Repr]"""
    }

    val clsName = TypeName(c.freshName())
    q"""
      final class $clsName extends _root_.derive.Derive.Generic[$tpe] {
        type Repr = ${deriveReprTypTree(tpe)}
        def to(p: $tpe): Repr = $to
        def from(p: Repr): $tpe = _root_.derive.Derive.Generic.unsafeGet(p).asInstanceOf[$tpe]
      }
      new $clsName(): _root_.derive.Derive.Generic.Aux[$tpe, ${deriveReprTypTree(tpe)}]
    """
  }

  def materializeDeriveGeneric[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]

    if(isProduct(tpe))
      mkProductDeriveGeneric(tpe)
    else if (isCoproduct(tpe))
      mkCoproductDeriveGeneric(tpe)
    else
      abort(s"$tpe is not case class like or the root of a sealed family of types")
  }

}


@macrocompat.bundle
trait DeriveTypes {
  type C <: whitebox.Context
  val c: C

  import c.universe._

  def deriveTpe: Type = typeOf[Derive[_]].typeConstructor

  object DeriveTpe {
    def unapply(tpe: Type): Option[(Type, Type)] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(derivedTpe))
          if cpdTpe.asType.toType.typeConstructor =:= deriveTpe =>
          derivedTpe.dealias match {
            case TypeRef(_, tcTpe, List(dTpe)) => Some((tcTpe.asType.toType.typeConstructor, dTpe))
            case _ => None
          }

        case _ => None
      }
  }

}

trait DeriveLazyExtension extends DeriveTypes with LazyExtension with CaseClassMacros {
  type C = ctx.c.type
  lazy val c: C = ctx.c

  import ctx._
  import c.universe._

  case object ThisState
  type ThisState = ThisState.type

  def id = "derive"

  def initialState = ThisState

  private type Maybe[T] = Either[String, (State, T)]

  def unitTpe = typeOf[Unit]
  def tuple2Tpe = typeOf[(_, _)].typeConstructor
  def nothingTpe = typeOf[Nothing]
  def eitherTpe = typeOf[Either[_, _]]

  def customReprTyp(tpe: Type): Type =
    if (isProduct(tpe)) fieldsOf(tpe).map(_._2).foldRight(unitTpe)(appliedType(tuple2Tpe, _, _))
    else ctorsOf(tpe).foldRight(nothingTpe)(appliedType(eitherTpe, _, _))

  def maybeImplicitValue(state: State, tpe: Type): Maybe[Tree] =
    ctx.State.resolveInstance(state)(tpe).toRight(s"Implicit not found: $tpe")

  def buildCall(
    tcTpe: Type,
    method: MethodSymbol,
    state: State,
    substTpe: Type => Type = identity,
    firstOpt0: => Option[Tree] = None )(
    buildArg: PartialFunction[String, (State, Symbol) => Maybe[Tree]]
  ): Maybe[Tree] = {
    val paramss = method.paramLists

    def canForceFirst = paramss
      .headOption
      .exists(_.headOption.exists(!_.isImplicit))

    val firstOpt = firstOpt0.filter(_ => canForceFirst)

    val maybeArgss = firstOpt.fold(paramss)(_ => paramss.tail)
      .foldLeft[Maybe[List[List[Tree]]]](Right((state, firstOpt.map(List(_)).toList))) { (acc, params) =>
        acc.right.flatMap { case (state0, treess) =>

          val maybeTrees = params.foldLeft[Maybe[List[Tree]]](Right((state0, Nil))) { (acc, param) =>
            acc.right.flatMap { case (state1, trees) =>
              val argName = param.name.decodedName.toString

              val maybeArg =
                if (param.isImplicit)
                  maybeImplicitValue(state1, substTpe(param.info))
                else if (buildArg.isDefinedAt(argName))
                  buildArg(argName)(state1, param)
                else
                  Left(s"Unrecognized argument in $method: $argName")

              maybeArg.right.map { case (state2, tree) =>
                (state2, tree :: trees)
              }
            }
          }

          maybeTrees.right.map{ case (s, trees) => (s, trees :: treess) }
        }

      }

    maybeArgss.right.map { case (state0, reverseArgss) =>
      state0 -> reverseArgss
        .map(_.reverse)
        .reverse
        .foldLeft(q"${companionRef(tcTpe)}.typeClass.$method")((acc, args) => q"$acc(..$args)")
    }
  }

  val Annotation = "([a-zA-Z]*)Annotation".r

  def genericRecursiveTc(
    tcTpe: Type,
    tpe: Type,
    typeClass: Symbol,
    state: State,
    isProduct: Boolean,
    prod: => Either[String, Symbol],
    labelTpes: List[(String, Type, Int)]
  ): Maybe[Tree] =
    labelTpes match {
      case Nil =>
        for {
          pointSym <- typeClass
            .info
            .members
            .find(sym => sym.name.decodedName.toString == "point" && sym.isMethod)
            .toRight(s"No point method found in $typeClass")
            .right

          stateCall <- buildCall(
            tcTpe,
            pointSym.asMethod,
            state,
            _.substituteTypes(pointSym.asMethod.typeParams.take(1), List(if (isProduct) unitTpe else nothingTpe)),
            Some(if (isProduct) q"()" else q"throw new _root_.java.util.NoSuchElementException")
          )(PartialFunction.empty).right

        } yield stateCall

      case List((lab, tpe0, idx), remaining @ _*) =>

        def subst(method: Symbol) =
          (t: Type) => t.substituteTypes(
            method.asMethod.typeParams.take(2),
            List(
              tpe0,
              remaining.map(_._2).foldLeft(if (isProduct) unitTpe else nothingTpe)(appliedType(if (isProduct) tuple2Tpe else eitherTpe, _, _))
            )
          )

        for {
          prodSym <- prod.right

          stateCall <- buildCall(tcTpe, prodSym.asMethod, state, subst(prodSym)) {
            case "head" => (state, param) =>
              val tpe1 =
                if (param.asTerm.isByNameParam) param.info match { case TypeRef(_, _, List(tpe0)) => tpe0 }
                else param.info

              ctx.derive(state)(subst(prodSym)(tpe1)).right.map { case (state0, inst) => state0 -> inst.ident }

            case "tail" => (state, _) => genericRecursiveTc(tcTpe, tpe, typeClass, state, isProduct, prod, remaining.toList)

            case "name" => (state, _) => Right((state, q"$lab"))

            case "default" =>
              val defaultMethodName = TermName(s"apply$$default$$$idx")
              val tree = tpe.companion.member(defaultMethodName) match {
                case NoSymbol => q"_root_.scala.None"
                case defaultValue => q"_root_.scala.Some(${companionRef(tpe)}.$defaultMethodName)"
              }

              (state, param) => Right(state -> tree)

            case Annotation(annotation) =>
              val treesOpt =
                if (isProduct) {
                  val paramOpt = tpe
                    .member(termNames.CONSTRUCTOR)
                    .asMethod
                    .paramLists
                    .flatten
                    .find(sym => nameAsString(sym.name) == lab)

                  paramOpt.flatMap { sym =>
                    sym.annotations.collectFirst {
                      case ann if nameOf(ann.tree.tpe).toString == annotation => ann.tree.children.tail
                    }
                  }
                } else
                  tpe0.typeSymbol.annotations.collectFirst {
                    case ann if nameOf(ann.tree.tpe).toString == annotation => ann.tree.children.tail
                  }

              val tree = treesOpt match {
                case None => q"_root_.scala.None"
                case Some(trees) =>
                  val arg = trees match {
                    case Nil => q"()"
                    case Seq(t) => t
                    case trees => q"(..$trees)"
                  }

                  q"_root_.scala.Some($arg)"
              }

              (state, _) => Right(state -> tree)

          }.right

        } yield stateCall
    }

  def recursiveTc(tcTpe: Type, tpe: Type, typeClass: Symbol, isProduct0: Boolean)(state: State): Maybe[Tree] = {
    val labelTpes =
      if (isProduct0) {
        if (tpe =:= typeOf[Unit] || isCaseObjectLike(tpe.typeSymbol.asClass))
          Nil
        else
          fieldsOf(tpe).map { case (name, tpe0) => nameAsString(name) -> devarargify(tpe0) }
      } else
        ctorsOf(tpe).map { tpe0 => nameAsString(nameOf(tpe0)) -> tpe0 }

    val prodMethodName = if (isProduct0) "product" else "sum"

    lazy val prod = typeClass
      .info
      .members
      .find(sym => sym.name.decodedName.toString == prodMethodName && sym.isMethod)
      .toRight(s"No $prodMethodName method found in $typeClass")

    // Ensuring the product or sum method exists prior to deriving any typeclass for a product or sum - even if the
    // product is empty
    prod.right.flatMap(_ =>
      genericRecursiveTc(
        tcTpe,
        tpe,
        typeClass,
        state,
        isProduct0,
        prod,
        labelTpes.zipWithIndex.map{ case ((lab, tpe0), idx) => (lab, tpe0, idx + 1) }
      )
    )
  }

  def genericMethod(tpe: Type, methodName: String)(state: State): Maybe[Tree] =
    ctx.derive(state)(appliedType(typeOf[Derive.Generic[_]].typeConstructor, tpe))
      .right.map { case (state0, inst) =>
        state0 -> q"${inst.ident}.${TermName(methodName)}"
      }

  def isProductIfGeneric(tpe: Type) =
    if (isProduct(tpe)) Right(true)
    else if (isCoproduct(tpe)) Right(false)
    else Left(s"$tpe is not case class like or the root of a sealed family of types")

  def doDerive(baseTpe: Type, tcTpe: Type, dTpe: Type, state: State) =
    for {
      isProduct1 <- isProductIfGeneric(dTpe).right

      typeClassSym <- tcTpe // FIXME Should companionRef be used here?
        .companion
        .members
        .find(sym => sym.name.decodedName.toString == "typeClass" && sym.isTerm)
        .toRight(s"No typeClass member found in $tcTpe companion")
        .right

      mapSym0 <- typeClassSym
        .info // FIXME Use infoIn? what type site should be supplied then?
        .members
        .find(sym => sym.name.decodedName.toString == "map" && sym.isMethod)
        .toRight(s"No map method found in typeClass member of $tcTpe companion")
        .right

      stateCall <- buildCall(tcTpe, mapSym0.asMethod, state, _.substituteTypes(mapSym0.asMethod.typeParams.take(2), List(dTpe, customReprTyp(dTpe)))) {
        case "from" => (state, _) => genericMethod(dTpe, "from")(state)
        case "to" => (state, _) => genericMethod(dTpe, "to")(state)
        case "tc" | "underlying" => (state, _) => recursiveTc(tcTpe, dTpe, typeClassSym, isProduct1)(state)
      } .right

    } yield {
      val (state0, call) = stateCall
      state0.closeInst(baseTpe, q"_root_.derive.Derive($call)", baseTpe)
    }

  def derive(
    state: State,
    extState: ThisState,
    update: (State, ThisState) => State )(
    tpe: Type
  ): Option[Either[String, (State, Instance)]] =
    tpe match {
      case DeriveTpe(tcTpe, dTpe) =>
        Some(state.lookup(tpe).left.flatMap(doDerive(tpe, tcTpe, dTpe, _)))
      case _ => None
    }

}
