package ar.com.flow.fp.monoids

import org.scalatest.{FunSpec, Matchers}

class OptionMonoidTest extends FunSpec with Matchers {
  val optionMonoid = Monoids.optionMonoid[String]

  describe("Option Monoid") {
    describe("Op") {
      it("should associative three Some operators") {
        val v1 = Some("Happy")
        val v2 = Some("New")
        val v3 = Some("Year")

        optionMonoid.op(optionMonoid.op(v1, v2), v3) shouldBe optionMonoid.op(v1, optionMonoid.op(v2, v3))
      }
      it("should associative Some, Some and None") {
        val v1 = Some("Happy")
        val v2 = Some("New")
        val v3 = None

        optionMonoid.op(optionMonoid.op(v1, v2), v3) shouldBe optionMonoid.op(v1, optionMonoid.op(v2, v3))
      }
      it("should associative Some, None and Some") {
        val v1 = Some("Happy")
        val v2 = None
        val v3 = Some("Year")

        optionMonoid.op(optionMonoid.op(v1, v2), v3) shouldBe optionMonoid.op(v1, optionMonoid.op(v2, v3))
      }
      it("should associative None, Some and Some") {
        val v1 = None
        val v2 = Some("New")
        val v3 = Some("Year")

        optionMonoid.op(optionMonoid.op(v1, v2), v3) shouldBe optionMonoid.op(v1, optionMonoid.op(v2, v3))
      }
      it("should associative Some, None and None") {
        val v1 = Some("Happy")
        val v2 = None
        val v3 = None

        optionMonoid.op(optionMonoid.op(v1, v2), v3) shouldBe optionMonoid.op(v1, optionMonoid.op(v2, v3))
      }
      it("should associative None, Some and None") {
        val v1 = None
        val v2 = Some("New")
        val v3 = None

        optionMonoid.op(optionMonoid.op(v1, v2), v3) shouldBe optionMonoid.op(v1, optionMonoid.op(v2, v3))
      }
      it("should associative None, None and Some") {
        val v1 = None
        val v2 = None
        val v3 = Some("Year")

        optionMonoid.op(optionMonoid.op(v1, v2), v3) shouldBe optionMonoid.op(v1, optionMonoid.op(v2, v3))
      }
      it("should associative None, None and None") {
        val v1 = None
        val v2 = None
        val v3 = None

        optionMonoid.op(optionMonoid.op(v1, v2), v3) shouldBe optionMonoid.op(v1, optionMonoid.op(v2, v3))
      }
    }
    describe("Zero") {
      it("should associate Some") {
        val v1 = Some("Happy")

        optionMonoid.op(v1, optionMonoid.zero) shouldBe v1
        optionMonoid.op(optionMonoid.zero, v1) shouldBe v1
      }

      it("should associate None") {
        optionMonoid.op(None, optionMonoid.zero) shouldBe None
        optionMonoid.op(optionMonoid.zero, None) shouldBe None
      }
    }
  }

}
