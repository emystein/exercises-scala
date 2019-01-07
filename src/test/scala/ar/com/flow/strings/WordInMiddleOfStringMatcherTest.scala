package ar.com.flow.strings

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class WordInMiddleOfStringMatcherTest extends FunSpec with TableDrivenPropertyChecks with Matchers {
  val table = Table(
    ("String", "Expected"),
    ("", false),
    ("1", true),
    ("1 2", false),
    ("1 2 3", true),
    ("12 3", false),
    ("1 23 4", true),
    ("1 234 5 ", true),
  )

  describe("Word in middle of string matcher") {
    it("should match") {
      forAll(table) { (input: String, expected: Boolean) =>
        StringAnalysis.wordInMiddle(input) shouldBe expected
      }
    }
  }
}
