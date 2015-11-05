package derive

import shapeless.Lazy
import org.scalatest._

object DeriveTestsDefinitions {

  case class CC(i: Int, s: String = "b")

  sealed trait Base
  case class BaseI(i: Int) extends Base
  case class BaseS(s: String) extends Base
  case object BaseObj extends Base


  object Simple {
    trait TC[T] {
      def repr: String
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](s: => String): TC[T] =
        new TC[T] {
          def repr = s
        }

      object typeClass {
        def point[A]: TC[A] = instance(".")
        def product[A, B](head: TC[A], tail: TC[B]): TC[(A, B)] = 
          instance(s"${head.repr},${tail.repr}")
        def sum[A, B](head: TC[A], tail: => TC[B]): TC[Either[A, B]] =
          instance(s"${head.repr}+${tail.repr}")
        def map[F, G](tc: TC[G]): TC[F] =
          instance(s"<${tc.repr}>")
      }

      implicit val intTC: TC[Int] = instance("int")
      implicit val stringTC: TC[String] = instance("string")

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[TC[T]]]): TC[T] =
        lzDerive.value.value
    }
  }

  object PointArg {
    trait TC[T] {
      def repr: String
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](s: => String): TC[T] =
        new TC[T] {
          def repr = s
        }

      object typeClass {
        def point[A](a: => A): TC[A] = instance(try a.toString catch { case _: Exception => "exception" })
        def product[A, B](head: TC[A], tail: TC[B]): TC[(A, B)] =
          instance(s"${head.repr},${tail.repr}")
        def sum[A, B](head: TC[A], tail: => TC[B]): TC[Either[A, B]] =
          instance(s"${head.repr}+${tail.repr}")
        def map[F, G](tc: TC[G]): TC[F] =
          instance(s"<${tc.repr}>")
      }

      implicit val intTC: TC[Int] = instance("int")
      implicit val stringTC: TC[String] = instance("string")

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[TC[T]]]): TC[T] =
        lzDerive.value.value
    }
  }

  object Name {
    trait TC[T] {
      def repr: String
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](s: => String): TC[T] =
        new TC[T] {
          def repr = s
        }

      object typeClass {
        def point[A]: TC[A] = instance(".")
        def product[A, B](head: TC[A], tail: TC[B], name: String): TC[(A, B)] =
          instance(s"$name:${head.repr},${tail.repr}")
        def sum[A, B](head: TC[A], tail: => TC[B], name: String): TC[Either[A, B]] =
          instance(s"$name:${head.repr}+${tail.repr}")
        def map[F, G](tc: TC[G]): TC[F] =
          instance(s"<${tc.repr}>")
      }

      implicit val intTC: TC[Int] = instance("int")
      implicit val stringTC: TC[String] = instance("string")

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[TC[T]]]): TC[T] =
        lzDerive.value.value
    }
  }

  object PointImplicit {
    trait Ev {
      def value: String
    }

    trait TC[T] {
      def repr: String
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](s: => String): TC[T] =
        new TC[T] {
          def repr = s
        }

      object typeClass {
        def point[A](implicit ev: Ev): TC[A] = instance(ev.value)
        def product[A, B](head: TC[A], tail: TC[B]): TC[(A, B)] =
          instance(s"${head.repr},${tail.repr}")
        def sum[A, B](head: TC[A], tail: => TC[B]): TC[Either[A, B]] =
          instance(s"${head.repr}+${tail.repr}")
        def map[F, G](tc: TC[G]): TC[F] =
          instance(s"<${tc.repr}>")
      }

      implicit val intTC: TC[Int] = instance("int")
      implicit val stringTC: TC[String] = instance("string")

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[TC[T]]]): TC[T] =
        lzDerive.value.value
    }
  }

  /** head and tail in product and sum have different type classes */
  object HeadTailTC {
    trait TC[T] {
      def repr: String
    }

    trait LowPriorityTC {
      implicit def fromSum[S](implicit tc: SumTC[S]): TC[S] =
        TC.instance(tc.repr)
    }
    
    object TC extends LowPriorityTC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](s: => String): TC[T] =
        new TC[T] {
          def repr = s
        }

      implicit val intTC: TC[Int] = instance("int")
      implicit val stringTC: TC[String] = instance("string")

      implicit def fromProduct[P](implicit tc: ProductTC[P]): TC[P] =
        instance(tc.repr)
    }


    trait ProductTC[T] { self =>
      def start: String
      def separator: String
      def end: String
      def items: Seq[String]

      def repr = items.mkString(start, separator, end)
      
      def prepend[U](item: String): ProductTC[U] =
        new ProductTC[U] {
          def start = self.start
          def separator = self.separator
          def end = self.end
          def items = item +: self.items
        }
      
      def to[U]: ProductTC[U] =
        new ProductTC[U] {
          def start = "<"
          def separator = self.separator
          def end = ">"
          def items = self.items
        }
    }

    object ProductTC {
      def apply[T](implicit tc: ProductTC[T]): ProductTC[T] = tc

      def instance[T](start0: String, separator0: String, end0: String, items0: Seq[String]): ProductTC[T] =
        new ProductTC[T] {
          def start = start0
          def separator = separator0
          def end = end0
          def items = items0
        }

      object typeClass {
        def point[A]: ProductTC[A] = instance("", ",", "", Nil)
        def product[A, B](head: TC[A], tail: ProductTC[B]): ProductTC[(A, B)] =
          tail.prepend(head.repr)
        def map[F, G](tc: ProductTC[G]): ProductTC[F] =
          tc.to
      }

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[ProductTC[T]]]): ProductTC[T] =
        lzDerive.value.value
    }


    trait SumTC[T] { self =>
      def start: String
      def separator: String
      def end: String
      def items: Seq[String]

      def repr = items.mkString(start, separator, end)

      def prepend[U](item: String): SumTC[U] =
        new SumTC[U] {
          def start = self.start
          def separator = self.separator
          def end = self.end
          def items = item +: self.items
        }

      def to[U]: SumTC[U] =
        new SumTC[U] {
          def start = "<"
          def separator = self.separator
          def end = ">"
          def items = self.items
        }
    }

    object SumTC {
      def apply[T](implicit tc: SumTC[T]): SumTC[T] = tc

      def instance[T](start0: String, separator0: String, end0: String, items0: Seq[String]): SumTC[T] =
        new SumTC[T] {
          def start = start0
          def separator = separator0
          def end = end0
          def items = items0
        }

      object typeClass {
        def point[A]: SumTC[A] = instance("", "+", "", Nil)
        def sum[A, B](head: TC[A], tail: => SumTC[B]): SumTC[(A, B)] =
          tail.prepend(head.repr)
        def map[F, G](tc: SumTC[G]): SumTC[F] =
          tc.to
      }

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[SumTC[T]]]): SumTC[T] =
        lzDerive.value.value
    }
  }

  object Default {
    trait TC[T] {
      def repr: String
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](s: => String): TC[T] =
        new TC[T] {
          def repr = s
        }

      object typeClass {
        def point[A]: TC[A] = instance(".")
        def product[A, B](head: TC[A], tail: TC[B], default: Option[A]): TC[(A, B)] =
          instance(s"${head.repr}${default.map("="+_).mkString},${tail.repr}")
        def sum[A, B](head: TC[A], tail: => TC[B]): TC[Either[A, B]] =
          instance(s"${head.repr}+${tail.repr}")
        def map[F, G](tc: TC[G]): TC[F] =
          instance(s"<${tc.repr}>")
      }

      implicit val intTC: TC[Int] = instance("int")
      implicit val stringTC: TC[String] = instance("string")

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[TC[T]]]): TC[T] =
        lzDerive.value.value
    }
  }

  object From {
    trait TC[T] {
      def build: T
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](t: => T): TC[T] =
        new TC[T] {
          def build = t
        }

      object typeClass {
        def point[A](a: => A): TC[A] = instance(a)
        def product[A, B](head: TC[A], tail: TC[B]): TC[(A, B)] =
          instance((head.build, tail.build))
        def sum[A, B](head: TC[A], tail: => TC[B]): TC[Either[A, B]] =
          instance(Left(head.build))
        def map[F, G](tc: TC[G], from: G => F): TC[F] =
          instance(from(tc.build))
      }

      implicit val intTC: TC[Int] = instance(1)
      implicit val stringTC: TC[String] = instance("b")

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[TC[T]]]): TC[T] =
        lzDerive.value.value
    }
  }

  object To {
    trait TC[T] {
      def validate(t: T): Boolean
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](validate0: T => Boolean): TC[T] =
        new TC[T] {
          def validate(t: T) = validate0(t)
        }

      object typeClass {
        // Doesn't work if the argument of point is removed here
        def point[A](a: => A): TC[A] = instance(_ => true)
        def product[A, B](head: TC[A], tail: TC[B]): TC[(A, B)] =
          instance { case (h, t) => head.validate(h) && tail.validate(t) }
        def sum[A, B](head: TC[A], tail: => TC[B]): TC[Either[A, B]] =
          instance {
            case Left(h) => head.validate(h)
            case Right(t) => tail.validate(t)
          }
        def map[F, G](tc: TC[G], to: F => G): TC[F] =
          instance(f => tc.validate(to(f)))
      }

      implicit val intTC: TC[Int] = instance(_ == 1)
      implicit val stringTC: TC[String] = instance(_ == "b")

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[TC[T]]]): TC[T] =
        lzDerive.value.value
    }
  }

  object ProductImplicitArg {
    trait TC0[T] {
      def extra: String
    }

    object TC0 {
      implicit val intTC0: TC0[Int] =
        new TC0[Int] {
          def extra = "0"
        }
      implicit val stringTC0: TC0[String] =
        new TC0[String] {
          def extra = "s"
        }
    }

    trait TC[T] {
      def repr: String
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](s: => String): TC[T] =
        new TC[T] {
          def repr = s
        }

      object typeClass {
        def point[A]: TC[A] = instance(".")
        def product[A, B](head: TC[A], tail: TC[B])(implicit tc0: TC0[A]): TC[(A, B)] =
          instance(s"${head.repr}${tc0.extra},${tail.repr}")
        def sum[A, B](head: TC[A], tail: => TC[B]): TC[Either[A, B]] =
          instance(s"${head.repr}+${tail.repr}")
        def map[F, G](tc: TC[G]): TC[F] =
          instance(s"<${tc.repr}>")
      }

      implicit val intTC: TC[Int] = instance("int")
      implicit val stringTC: TC[String] = instance("string")

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[TC[T]]]): TC[T] =
        lzDerive.value.value
    }
  }

  object SumImplicitArg {
    trait TC0[T] {
      def extra: String
    }

    object TC0 {
      implicit val baseITC0: TC0[BaseI] =
        new TC0[BaseI] {
          def extra = "1"
        }
      implicit val baseSTC0: TC0[BaseS] =
        new TC0[BaseS] {
          def extra = "2"
        }
      implicit val baseObjTC0: TC0[BaseObj.type] =
        new TC0[BaseObj.type] {
          def extra = "3"
        }
    }

    trait TC[T] {
      def repr: String
    }

    object TC {
      def apply[T](implicit tc: TC[T]): TC[T] = tc

      def instance[T](s: => String): TC[T] =
        new TC[T] {
          def repr = s
        }

      object typeClass {
        def point[A]: TC[A] = instance(".")
        def product[A, B](head: TC[A], tail: TC[B]): TC[(A, B)] =
          instance(s"${head.repr},${tail.repr}")
        def sum[A, B](head: TC[A], tail: => TC[B])(implicit tc0: TC0[A]): TC[Either[A, B]] =
          instance(s"${head.repr}${tc0.extra}+${tail.repr}")
        def map[F, G](tc: TC[G]): TC[F] =
          instance(s"<${tc.repr}>")
      }

      implicit val intTC: TC[Int] = instance("int")
      implicit val stringTC: TC[String] = instance("string")

      implicit def derivedTC[T](implicit lzDerive: Lazy[Derive[TC[T]]]): TC[T] =
        lzDerive.value.value
    }
  }

}

class DeriveTests extends FlatSpec with Matchers {

  import DeriveTestsDefinitions._

  "Derive" should "derive type classes for simple types" in {
    import Simple._

    assert(TC[Int].repr == "int")
    assert(TC[String].repr == "string")

    assert(TC[CC].repr == "<int,string,.>")
    assert(TC[Base].repr == "<<int,.>+<.>+<string,.>+.>")
  }

  it should "take point argument into account" in {
    import PointArg._

    assert(TC[Int].repr == "int")
    assert(TC[String].repr == "string")

    assert(TC[CC].repr == "<int,string,()>")
    assert(TC[Base].repr == "<<int,()>+<()>+<string,()>+exception>")
  }

  it should "provide names" in {
    import Name._

    assert(TC[Int].repr == "int")
    assert(TC[String].repr == "string")

    assert(TC[CC].repr == "<i:int,s:string,.>")
    assert(TC[Base].repr == "<BaseI:<i:int,.>+BaseObj:<.>+BaseS:<s:string,.>+.>")
  }

  it should "allow point to look for implicits" in {
    import PointImplicit._

    implicit val ev =
      new Ev {
        def value = "0"
      }

    assert(TC[Int].repr == "int")
    assert(TC[String].repr == "string")

    assert(TC[CC].repr == "<int,string,0>")
    assert(TC[Base].repr == "<<int,0>+<0>+<string,0>+0>")
  }

  it should "allow head arguments to look for a different type class for head" in {
    import HeadTailTC._

    assert(TC[Int].repr == "int")
    assert(TC[String].repr == "string")

    assert(TC[CC].repr == "<int,string>")
    assert(TC[Base].repr == "<<int>+<>+<string>>")
  }

  it should "provide default values" in {
    import Default._

    assert(TC[Int].repr == "int")
    assert(TC[String].repr == "string")

    assert(TC[CC].repr == "<int,string=b,.>")
    assert(TC[Base].repr == "<<int,.>+<.>+<string,.>+.>")
  }

  it should "provide a from argument to the generic method" in {
    import From._

    assert(TC[Int].build == 1)
    assert(TC[String].build == "b")

    assert(TC[CC].build == CC(1, "b"))
    assert(TC[Base].build == BaseI(1))
  }

  it should "provide a to argument to the generic method" in {
    import To._

    assert(!TC[Int].validate(0))
    assert(TC[Int].validate(1))
    assert(!TC[String].validate(""))
    assert(TC[String].validate("b"))

    assert(!TC[CC].validate(CC(1, "")))
    assert(!TC[CC].validate(CC(0, "b")))
    assert(TC[CC].validate(CC(1, "b")))
    assert(!TC[Base].validate(BaseI(0)))
    assert(TC[Base].validate(BaseI(1)))
    assert(!TC[Base].validate(BaseS("")))
    assert(TC[Base].validate(BaseS("b")))
    assert(TC[Base].validate(BaseObj))
  }

  it should "allow product to look for other implicits" in {
    import ProductImplicitArg._

    assert(TC[Int].repr == "int")
    assert(TC[String].repr == "string")

    assert(TC[CC].repr == "<int0,strings,.>")
    assert(TC[Base].repr == "<<int0,.>+<.>+<strings,.>+.>")
  }

  it should "allow sum to look for other implicits" in {
    import SumImplicitArg._

    assert(TC[Int].repr == "int")
    assert(TC[String].repr == "string")

    assert(TC[CC].repr == "<int,string,.>")
    assert(TC[Base].repr == "<<int,.>1+<.>3+<string,.>2+.>")
  }

}
