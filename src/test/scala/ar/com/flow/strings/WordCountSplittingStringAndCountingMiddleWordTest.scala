package ar.com.flow.strings

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

class WordCountSplittingStringAndCountingMiddleWordTest extends FunSuite with TableDrivenPropertyChecks with Matchers {
  test("Recursive word count") {
    forAll(WordCountTestData.table) { (input: String, expected: Int) =>
      WordCountSplittingStringAndCountingMiddleWord.apply(input) shouldBe expected
    }
  }
}
