package ar.com.flow.fp.monoids

import ar.com.flow.strings.WordCountTestData
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

class WordCountUsingMonoidTest extends FunSuite with TableDrivenPropertyChecks with Matchers {
  test("Word count using WordCount monoid") {
    forAll(WordCountTestData.table) { (input: String, expected: Int) =>
      WordCountUsingMonoid.apply(input) shouldBe expected
    }
  }
}
