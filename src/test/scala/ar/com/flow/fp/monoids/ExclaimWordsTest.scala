package ar.com.flow.fp.monoids

import org.scalatest.{FunSpec, Matchers}

class ExclaimWordsTest extends FunSpec with Matchers {
  describe("Exclaim words") {
    describe("when sentence has many words") {
      it("should exclaim every word") {
        Strings.exclaimWords("No de nuevo") shouldBe "No! de! nuevo!"
      }
    }
    describe("when sentence has one word") {
      it("should exclaim the word") {
        Strings.exclaimWords("No") shouldBe "No!"
      }
    }
    describe("when sentence has no word") {
      it("should not exclaim") {
        Strings.exclaimWords("") shouldBe ""
      }
    }
  }
}
