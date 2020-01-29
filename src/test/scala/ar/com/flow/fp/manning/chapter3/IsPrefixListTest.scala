package ar.com.flow.fp.manning.chapter3

import Chapter3Exercises._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FunSuite, Matchers}

class IsPrefixListTest extends FunSuite with Matchers {
  val successValues = Table(
    ("prefix", "list"),
    (Nil, Nil),
    (Nil, List(1)),
    (List(1), List(1)),
    (List(1), List(1, 2)),
    (List(1, 2), List(1, 2)),
    (List(1, 2), List(1, 2, 3)),
  )

  forAll(successValues) { (prefix: List[Int], list: List[Int]) =>
    isPrefix(prefix, list) shouldBe true
  }

  val failValues = Table(
    ("prefix", "list"),
    (List(1), Nil),
    (List(1), List(2)),
    (List(1, 2), List(1)),
    (List(1, 2), List(1, 3)),
    (List(1, 2, 3), List(1, 2, 4)),
  )

  forAll(failValues) { (prefix: List[Int], list: List[Int]) =>
    isPrefix(prefix, list) shouldBe false
  }
}
