package ar.com.flow.fp.monoids

import ar.com.flow.Monoids.{Part, Stub, wcMonoid}
import org.scalatest.{FunSpec, Matchers}

class WordCountMonoidTest extends FunSpec with Matchers {

  describe("Word Count Monoid") {
    describe("Combine two Stubs") {
      it("should combine them into a Part with left part the first Stub string and the right part the second Stub string") {
        val stub1 = Stub("Un banco")
        val stub2 = Stub("sin sucursales")

        wcMonoid.op(stub1, stub2) shouldBe Part("Un banco", 0, "sin sucursales")
      }
    }
    describe("Combine a first Part with only left stub and second Part with only right stub") {
      it("should combine them into a single Part with original left and right stubs") {
        val part1 = Part("Un banco", 0, "")
        val part2 = Part("", 0, "sin sucursales")

        wcMonoid.op(part1, part2) shouldBe Part("Un banco", 0, "sin sucursales")
      }
    }
    describe("Combine two Parts") {
      it("should sum the word count") {
        val part1 = Part("Un banco", 1, "")
        val part2 = Part("", 2, "sin sucursales")

        wcMonoid.op(part1, part2) shouldBe Part("Un banco", 3, "sin sucursales")
      }
    }
    describe("Associativity") {
      it("should associate three parts") {
        val part1 = Part("Un", 1, "")
        val part2 = Part("", 1, "banco")
        val part3 = Part("", 1, "sin sucursales")

        wcMonoid.op(wcMonoid.op(part1, part2), part3) shouldBe wcMonoid.op(part1, wcMonoid.op(part2, part3))
      }
    }
  }
}
