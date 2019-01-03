package ar.com.flow.fp.monoids

import org.scalatest.{FunSpec, Matchers}
import Monoids._

class BalancedFoldMapTest extends FunSpec with Matchers {

  describe("Balanced foldMap") {
    describe("when list is empty") {
      it("should return Monoid zero") {
        foldMapV(List.empty, stringConcatenateMonoid)(identity) shouldBe ""
      }
    }
    describe("when list is not empty") {
      it("should return mapped fold") {
        foldMapV(List("lorem", "ipsum", "dolor", "sit"), stringConcatenateMonoid)(identity) shouldBe "loremipsumdolorsit"
      }
    }
  }
}
