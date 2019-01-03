package ar.com.flow.fp.monoids

import ar.com.flow.fp.monoids.Strings._
import org.scalatest.{FunSpec, Matchers}

class EndoMonoidTest extends FunSpec with Matchers {
  def endoMonoid: Monoid[String => String] = Monoids.endoMonoid[String]

  describe("EndoMonoid") {
    describe("Zero") {
      it("should associate input function and zero") {
        val result = endoMonoid.op(toUpperCase, endoMonoid.zero)

        result("asdf") shouldBe toUpperCase("asdf")
      }
      it("should associate zero and input function") {
        val result = endoMonoid.op(endoMonoid.zero, toUpperCase)

        result("asdf") shouldBe toUpperCase("asdf")
      }
    }
    describe("Op") {
      it("should associate three functions") {
        val result = endoMonoid.op(replaceSpaceWithUnderscore, endoMonoid.op(toUpperCase, exclaimWords))
        result("Happy New Year") shouldBe "HAPPY_NEW_YEAR!"

        val result2 = endoMonoid.op(endoMonoid.op(replaceSpaceWithUnderscore, toUpperCase), exclaimWords)
        result2("Happy New Year") shouldBe "HAPPY_NEW_YEAR!"
      }
    }
  }
}
