package derive

import org.scalatest._

class ImplicitTests extends FlatSpec with Matchers {

  trait TC[T] {
    def msg: String
  }

  object TC {
    implicit val int: TC[Int] =
      new TC[Int] {
        def msg = "int"
      }
  }

  "Implicit" should "fill Option[T] with implicits" in {
    val int = Implicit[Option[TC[Int]]].value
    assert(int.nonEmpty)
    assert(int.get.msg == "int")

    val bool = Implicit[Option[TC[Boolean]]].value
    assert(bool.isEmpty)
  }

  it should "fill Either[L, R] with implicits" in {
    val either = Implicit[Either[TC[Int], TC[Boolean]]].value
    assert(either.isLeft)
    assert(either.left.get.msg == "int")
  }

}
