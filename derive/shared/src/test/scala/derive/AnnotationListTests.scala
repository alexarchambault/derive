package derive

import shapeless._
import org.scalatest._

import scala.annotation.StaticAnnotation

class AnnotationListTests extends FlatSpec with Matchers {

  case class Ann(s: String) extends StaticAnnotation
  case class CC(
    i: Int,
    @Ann("a") @Ann("b") s: String
  )

  "Annotations" should "find simple annotations" in {
    val l = AnnotationList[Ann, CC].apply()
    val l0: Nil.type :: scala.::[Ann] :: HNil = l
    val (_ :: annotations :: HNil) = l
    val expectedAnnotations = List(
      new Ann("a"),
      new Ann("b")
    )
    assert(annotations == expectedAnnotations)
  }

}
