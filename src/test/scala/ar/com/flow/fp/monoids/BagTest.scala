package ar.com.flow.fp.monoids

import org.scalatest.{FunSuite, Matchers}

class BagTest extends FunSuite with Matchers {
  test("BagTest") {
    val expectedMap = Map("a" -> 2, "rose" -> 2, "is" -> 1)

    Bag.bag(Vector("a", "rose", "is", "a", "rose")) shouldBe expectedMap
  }
}
