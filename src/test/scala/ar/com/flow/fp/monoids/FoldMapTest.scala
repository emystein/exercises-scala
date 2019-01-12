package ar.com.flow.fp.monoids

import ar.com.flow.Numbers._
import ar.com.flow.fp.monoids.Monoids._
import org.scalatest.{FunSpec, Matchers}

class FoldMapTest extends FunSpec with Matchers {
  describe("foldMap") {
    describe("an empty list") {
      it("should return 0") {
        foldMap(List.empty, intSumMonoid)(multiplyBy2) shouldBe 0
      }
    }
    describe("a list with one element") {
      it("should return a one element mapped") {
        foldMap(List(1), intSumMonoid)(multiplyBy2) shouldBe 2
      }
    }
    describe("a list with more than one element") {
      it("should return all elements mapped") {
        foldMap(List(1, 2, 3), intSumMonoid)(multiplyBy2) shouldBe 12
      }
    }
  }
}
