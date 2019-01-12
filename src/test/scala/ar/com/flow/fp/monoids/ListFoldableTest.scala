package ar.com.flow.fp.monoids

import org.scalatest.{FunSpec, Matchers}

class ListFoldableTest extends FunSpec with Matchers {
  val foldable = new ListFoldable()

  describe("Foldable") {
    describe("foldLeft") {
      it("should fold") {
        val input = List("a", "bb", "ccc")

        foldable.foldLeft(input)(0)(_ + _.length) shouldBe 6
      }
    }
    describe("foldRight") {
      it("should fold") {
        val input = List("a", "bb", "ccc")

        foldable.foldRight(input)(0)(_.length + _) shouldBe 6
      }
    }
    describe("foldMap") {
      it("should fold") {
        val input = List("a", "bb", "ccc")

        foldable.foldMap(input)(identity)(Monoids.stringConcatenateMonoid) shouldBe "abbccc"
      }
    }
  }
}
