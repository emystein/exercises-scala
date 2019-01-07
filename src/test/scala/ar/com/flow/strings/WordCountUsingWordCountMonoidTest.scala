package ar.com.flow.strings

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class WordCountUsingWordCountMonoidTest extends FunSpec with TableDrivenPropertyChecks with Matchers {
  val table = Table(
    ("String", "Expected"),
    ("", 0),
    ("1", 1),
    ("1 2", 2),
    ("1 2 3", 3),
    ("12 3", 2),
    ("1 23 4", 3),
    ("1 234 5 ", 3),
  )

  describe("Word count using WordCount monoid") {
    it("should match") {
      forAll(table) { (input: String, expected: Int) =>
        WordCountUsingWordCountMonoid.apply(input) shouldBe expected
      }
    }
  }
}
