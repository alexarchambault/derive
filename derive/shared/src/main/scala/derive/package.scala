import shapeless._

package object derive {

  implicit def optionGeneric[T]: Generic.Aux[Option[T], Some[T] :+: None.type :+: CNil] =
    new Generic[Option[T]] {
      type Repr = Some[T] :+: None.type :+: CNil
      def from(r: Repr) = r.unify
      def to(opt: Option[T]) = opt match {
        case None => Inr(Inl(None))
        case s @ Some(_) => Inl(s)
      }
    }

  implicit def eitherGeneric[L, R]: Generic.Aux[Either[L, R], Right[L, R] :+: Left[L, R] :+: CNil] =
    new Generic[Either[L, R]] {
      type Repr = Right[L, R] :+: Left[L, R] :+: CNil
      def from(r: Repr) = r.unify
      def to(either: Either[L, R]) = either match {
        case r @ Right(_) => Inl(r)
        case l @ Left(_) => Inr(Inl(l))
      }
    }

}
