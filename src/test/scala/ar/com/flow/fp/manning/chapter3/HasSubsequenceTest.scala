package ar.com.flow.fp.manning.chapter3

import Chapter3Exercises.hasSubsequence
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FunSuite, Matchers}

class HasSubsequenceTest extends FunSuite with Matchers {
  val successData = Table(
    ("sub", "sup"),
    (List(1), List(1)),
    (List(1), List(1, 2)),
    (List(1), List(2, 1)),
    (List(1, 2), List(1, 2)),
    (List(1, 2), List(1, 2, 3)),
    (List(1, 2), List(0, 1, 2)),
    (List(1, 2), List(0, 1, 2, 3)),
    (List(1, 2), List(0, 1, 2, 3, 4)),
    (List(1, 2), List(0, 0, 1, 2, 3)),
  )

  forAll(successData) { (sub: List[Int], sup: List[Int]) =>
    hasSubsequence(sub, sup) shouldBe true
  }
}
