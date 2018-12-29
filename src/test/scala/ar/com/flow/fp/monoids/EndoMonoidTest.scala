package ar.com.flow.fp.monoids

import ar.com.flow.fp.monoids.Strings._
import org.scalatest.{FunSpec, Matchers}

class EndoMonoidTest extends FunSpec with Matchers {
  def endoMonoid = Monoids.endoMonoid[String]

  describe("EndoMonoid") {
    describe("Zero") {
      it("should associate input function and zero") {
        val result = endoMonoid.op(stringToUpperCase, endoMonoid.zero)

        result("asdf") shouldBe stringToUpperCase("asdf")
      }
      it("should associate zero and input function") {
        val result = endoMonoid.op(endoMonoid.zero, stringToUpperCase)

        result("asdf") shouldBe stringToUpperCase("asdf")
      }
    }
    describe("Op") {
      it("should associate three functions") {
        val result = endoMonoid.op(replaceSpaceWithUnderscore, endoMonoid.op(stringToUpperCase, exclaimWords))
        result("Happy New Year") shouldBe "HAPPY_NEW_YEAR!"

        val result2 = endoMonoid.op(endoMonoid.op(replaceSpaceWithUnderscore, stringToUpperCase), exclaimWords)
        result2("Happy New Year") shouldBe "HAPPY_NEW_YEAR!"
      }
    }
  }
}
