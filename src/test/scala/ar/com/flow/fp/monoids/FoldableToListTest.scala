package ar.com.flow.fp.monoids

import org.scalatest.{FunSuite, Matchers}

class FoldableToListTest extends FunSuite with Matchers {
  val foldable = new TraversableFoldable

  test("Empty vector to List") {
    foldable.toList(Vector.empty) shouldBe List.empty
  }

  test("Vector with one element to List") {
    foldable.toList(Vector("a")) shouldBe List("a")
  }

  test("Vector with two elements to List") {
    foldable.toList(Vector("a", "b")) shouldBe List("a", "b")
  }
}
