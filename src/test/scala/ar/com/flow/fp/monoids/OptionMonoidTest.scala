package ar.com.flow.fp.monoids

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class OptionMonoidTest extends FunSpec with TableDrivenPropertyChecks with Matchers {
  val optionMonoid = Monoids.optionMonoid[String]

  val triples = Table(
    ("A", "B", "C"),
    (Some("Happy"), Some("New"), Some("Year")),
    (Some("Happy"), Some("New"), None),
    (Some("Happy"), None, Some("Year")),
    (None, Some("New"), Some("Year")),
    (Some("Happy"), None, None),
    (None, Some("New"), None),
    (None, None, Some("Year")),
    (None, None, None)
  )

  describe("Option Monoid") {
    describe("Op") {
      it("should associate values") {
        forAll(triples) { (v1: Option[String], v2: Option[String], v3: Option[String]) =>
          optionMonoid.op(optionMonoid.op(v1, v2), v3) shouldBe optionMonoid.op(v1, optionMonoid.op(v2, v3))
        }
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
