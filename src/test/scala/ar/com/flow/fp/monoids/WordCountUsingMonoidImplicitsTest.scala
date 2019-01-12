package ar.com.flow.fp.monoids

import ar.com.flow.strings.WordCountTestData
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

class WordCountUsingMonoidImplicitsTest extends FunSuite with TableDrivenPropertyChecks with Matchers {
  test("Word count using WC monoid and implicits") {
    forAll(WordCountTestData.table) { (input: String, expected: Int) =>
      WordCountUsingMonoidAndImplicits.apply(input) shouldBe expected
    }
  }
}
