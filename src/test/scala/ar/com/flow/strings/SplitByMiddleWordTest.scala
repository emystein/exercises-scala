package ar.com.flow.strings

import ar.com.flow.Strings
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class SplitByMiddleWordTest extends FunSpec with TableDrivenPropertyChecks with Matchers {
  val table = Table(
    ("String", "Expected"),
    ("", ("", "", "")),
    ("Hello", ("", "Hello", "")),
    ("Hello World", ("Hello", "", "World")),
    ("Hello Beautiful World", ("Hello ", "Beautiful", " World")),
    ("1 23", ("1 ", "", "23")),
  )

  describe("Split string by middle word") {
    it("should split word in left substring before middle word, middle word, and right substring after middle word") {
      forAll(table) { (input: String, expected: (String, String, String)) =>
        Strings.splitByMiddleWord(input) shouldBe expected
      }
    }
  }
}
