package ar.com.flow.fp.monoids

import org.scalatest.{FunSuite, Matchers}

class TraversableFoldableTest extends FunSuite with Matchers {
  val foldable = new TraversableFoldable()

  test("foldLeft on empty vector") {
    val input = Vector[String]()
    foldable.foldLeft(input)(0)(_ + _.length) shouldBe 0
  }
  test("foldLeft on vector with one element") {
    val input = Vector("a")
    foldable.foldLeft(input)(0)(_ + _.length) shouldBe 1
  }
  test("foldLeft on vector with more than one element") {
    val input = Vector("a", "bb", "ccc")
    foldable.foldLeft(input)(0)(_ + _.length) shouldBe 6
  }
  test("foldRight") {
    val input = Vector("a", "bb", "ccc")
    foldable.foldRight(input)(0)(_.length + _) shouldBe 6
  }
  test("foldMap") {
    val input = Vector("a", "bb", "ccc")
    foldable.foldMap(input)(identity)(Monoids.stringConcatenateMonoid) shouldBe "abbccc"
  }
}
