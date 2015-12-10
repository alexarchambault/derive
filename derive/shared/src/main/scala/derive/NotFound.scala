package derive

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import shapeless._

trait NotFound[T] extends Serializable

trait NotFoundMask[M, T] extends Serializable

object NotFound extends LazyExtensionCompanion {
  def apply[T](implicit nf: Strict[NotFound[T]]): NotFound[T] =
    nf.value


  implicit def init[T]: NotFound[T] = macro initImpl

  def instantiate(ctx0: DerivationContext): LazyExtension { type Ctx = ctx0.type } =
    new NotFoundLookupExtension {
      type Ctx = ctx0.type
      val ctx: ctx0.type = ctx0
    }
}

@macrocompat.bundle
trait NotFoundTypes {
  type C <: whitebox.Context
  val c: C

  import c.universe._


  def notFoundTpe: Type = typeOf[NotFound[_]].typeConstructor

  object NotFoundTpe {
    def unapply(tpe: Type): Option[Type] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(highTpe))
          if cpdTpe.asType.toType.typeConstructor =:= notFoundTpe =>
          Some(highTpe)
        case _ =>
          None
      }
  }

  def maskTpe: Type = typeOf[NotFoundMask[_, _]].typeConstructor

  object NotFoundMaskTpe {
    def unapply(tpe: Type): Option[(Type, Type)] =
      tpe.dealias match {
        case TypeRef(_, cpdTpe, List(mTpe, tTpe))
          if cpdTpe.asType.toType.typeConstructor =:= maskTpe =>
          Some(mTpe, tTpe)
        case _ =>
          None
      }
  }

}

trait NotFoundLookupExtension extends LazyExtension with NotFoundTypes {
  type C = ctx.c.type
  lazy val c: C = ctx.c

  import ctx._
  import c.universe._

  case class ThisState(
    priorityLookups: List[TypeWrapper]
  ) {
    def addPriorityLookup(tpe: Type): ThisState =
      copy(priorityLookups = TypeWrapper(tpe) :: priorityLookups)
    def removePriorityLookup(tpe: Type): ThisState =
      copy(priorityLookups = priorityLookups.filter(_ != TypeWrapper(tpe)))
  }

  def id = "not-found"

  def initialState = ThisState(Nil)

  def deriveNotFound(
    state: State,
    extState: ThisState,
    update: (State, ThisState) => State )(
    priorityTpe: Type,
    highInstTpe: Type,
    mask: String
  ): (State, Instance) = {
    val higherPriorityAvailable = {
      val extState1 = extState
        .addPriorityLookup(priorityTpe)
      val state1 = update(state, extState1)

      ctx.derive(state1)(highInstTpe)
        .right.toOption
        .flatMap{case (state2, inst) =>
          if (inst.inst.isEmpty)
            resolve0(state2)(highInstTpe)
              .map{case (_, tree, _) => tree }
          else
            Some(inst.inst.get)
        }
        .exists { actualTree =>
          mask.isEmpty || {
            actualTree match {
              case TypeApply(method, other) =>
                !method.toString().endsWith(mask)
              case _ =>
                true
            }
          }
        }
    }

    val highInstTpe0 =
      if (mask.isEmpty)
        highInstTpe
      else
        appliedType(maskTpe, List(internal.constantType(Constant(mask)), highInstTpe))

    val low =
      q"new _root_.derive.NotFound[$highInstTpe0] {}"
    val lowTpe0 = appliedType(notFoundTpe, List(highInstTpe0))

    if (higherPriorityAvailable)
      c.abort(c.enclosingPosition, s"$highInstTpe available elsewhere")
    else
      state.closeInst(priorityTpe, low, lowTpe0)
  }

  def derive(
    state0: State,
    extState: ThisState,
    update: (State, ThisState) => State )(
    instTpe0: Type
  ): Option[Either[String, (State, Instance)]] =
    instTpe0 match {
      case NotFoundTpe(highTpe) =>
        Some {
          if (extState.priorityLookups.contains(TypeWrapper(instTpe0)))
            Left(s"Not deriving $instTpe0")
          else
            state0.lookup(instTpe0).left.flatMap { state =>
              val eitherHighTpeNotFoundMask =
                highTpe match {
                  case NotFoundMaskTpe(mTpe, tTpe) =>
                    mTpe match {
                      case ConstantType(Constant(mask: String)) if mask.nonEmpty =>
                        Right((tTpe, mask))
                      case _ =>
                        Left(s"Unsupported mask type: $mTpe")
                    }
                  case _ =>
                    Right((highTpe, ""))
                }

              eitherHighTpeNotFoundMask.right.map{ case (highTpe, mask) =>
                deriveNotFound(state, extState, update)(instTpe0, highTpe, mask)
              }
            }
        }

      case _ => None
    }
}

