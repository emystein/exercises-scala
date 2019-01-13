package ar.com.flow.fp.monoids

import org.scalatest.{FunSpec, Matchers}

class ProductMonoidTest extends FunSpec with Matchers {
  describe("Product Monoid") {
    val m1 = Monoids.intSumMonoid
    val m2 = Monoids.intSumMonoid

    val productMonoid = Monoids.productMonoid(m1, m2)

    describe("Zero") {
      it("should be zero") {
        productMonoid.zero shouldBe(m1.zero, m2.zero)
      }
    }

    describe("Combination operation") {
      it("Should be combination of monoids combination operation") {
        productMonoid.op((1, 2), (1, 2)) shouldBe(2, 4)
      }
    }

    describe("Associativity") {
      it("should be associative") {
        productMonoid.op((1, 2), productMonoid.op((3, 4), (5, 6))) shouldBe productMonoid.op(productMonoid.op((1, 2), (3, 4)), (5, 6))
      }
    }
  }
}
