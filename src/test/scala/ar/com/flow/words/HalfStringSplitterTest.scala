package ar.com.flow.words

import org.scalatest.{FunSpec, Matchers}
import HalfStringSplitter.splitWithoutCuttingWords

class HalfStringSplitterTest extends FunSpec with Matchers {
  describe("Split string in halves without cutting words") {
    describe("Splitting empty string") {
      it("should return empty string") {
        splitWithoutCuttingWords("") shouldBe ("", "")
      }
    }
    describe("Splitting single word") {
      it("should return single word in the left half and empty right half") {
        splitWithoutCuttingWords("Simultaneous") shouldBe ("Simultaneous", "")
      }
    }
    describe("Splitting two words with middle being non-space") {
      it("should return two words in the left half and empty right half") {
        splitWithoutCuttingWords("The remedy") shouldBe ("The remedy", "")
      }
    }
    describe("Splitting two words with middle being a space") {
      it("should return the first word in the first half and the second word in the right half") {
        splitWithoutCuttingWords("Jamón Jamón") shouldBe ("Jamón", "Jamón")
      }
    }
    describe("Splitting three words string with middle of string being non-space") {
      it("should return two words in the left half and third word in the right half") {
        splitWithoutCuttingWords("A perfect circle") shouldBe ("A perfect", "circle")
      }
    }
  }

}
