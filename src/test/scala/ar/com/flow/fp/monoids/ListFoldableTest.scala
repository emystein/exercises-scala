package ar.com.flow.fp.monoids

import org.scalatest.{FunSuite, Matchers}

class ListFoldableTest extends FunSuite with Matchers {
  val foldable = new ListFoldable()

  test("foldLeft") {
    val input = List("a", "bb", "ccc")

    foldable.foldLeft(input)(0)(_ + _.length) shouldBe 6
  }
  test("foldRight") {
    val input = List("a", "bb", "ccc")

    foldable.foldRight(input)(0)(_.length + _) shouldBe 6
  }
  test("foldMap") {
    val input = List("a", "bb", "ccc")

    foldable.foldMap(input)(identity)(Monoids.stringConcatenateMonoid) shouldBe "abbccc"
  }
}
