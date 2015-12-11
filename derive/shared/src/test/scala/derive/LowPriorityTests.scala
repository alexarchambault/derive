package derive

import org.scalatest.{ FlatSpec, Matchers }
import shapeless.Strict

object LowPriorityTests {

  trait TC[T]

  object TC {
    def apply[T](implicit tc: TC[T]): TC[T] = tc

    implicit val intTC: TC[Int] = new TC[Int] {}
  }

  case class CC(s: String)

  object CC {
    implicit val ccTC: TC[CC] = new TC[CC] {}
  }

  case class CC2(s: String)

  trait Extra

  object Extra {
    implicit def extraTC[T](implicit notFound: LowPriority[TC[T]]): TC[T] =
      new TC[T] with Extra {}
  }

}

class LowPriorityTests extends FlatSpec with Matchers {
  import LowPriorityTests._

  "LowPriority" should "not prevent extra type class" in {
    import Extra._

    TC[Int] should not be a[Extra]
    TC[CC] should not be a[Extra]
    TC[String] shouldBe a[Extra]
    TC[CC2] shouldBe a[Extra]

    {
      implicit val cc2TC: TC[CC2] = new TC[CC2] {}
      TC[CC2] should not be a[Extra]
    }
  }

}
