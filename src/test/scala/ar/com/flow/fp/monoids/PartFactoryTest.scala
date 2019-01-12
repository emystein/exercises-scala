package ar.com.flow.fp.monoids

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

class PartFactoryTest extends FunSuite with TableDrivenPropertyChecks with Matchers {
  val table = Table(
    ("leftString", "middleWord", "rightString", "Expected"),
    ("", "", "", Part("", 0, "")),
    ("a", "", "", Part("a", 0, "")),
    ("", "a", "", Part("", 1, "")),
    ("", "", "a", Part("", 0, "a")),
    ("a", "b", "", Part("a", 1, "")),
    ("a", "", "b", Part("a", 0, "b")),
    ("", "a", "b", Part("", 1, "b")),
    ("a", "b", "c", Part("a", 1, "c")),
  )

  test("Part factory") {
    forAll(table) { (leftString: String, middleWord: String, rightString: String, expected: Part) =>
      Part.create(leftString, middleWord, rightString) shouldBe expected
    }
  }
}
