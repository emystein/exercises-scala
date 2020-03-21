package ar.com.flow.kata.pascaltriangle

import org.scalatest.{Matchers, WordSpec}

class RelativeNodesTest extends WordSpec with Matchers {
  val coordinates32 = Coordinates(3, 2)

  "Upper Row, Same Column relative Node" when {
    "Coordinates are (3, 2)" should {
      "have Coordinates (2, 2)" in {
        UpperRowSameColumnNode.from(coordinates32).coordinates shouldBe Coordinates(2, 2)
      }
    }
  }
  "Upper Row, Rightmost Column relative Node" when {
    "Coordinates are (3, 2)" should {
      "have Coordinates (2, 3)" in {
        UpperRowRightMostColumnNode.from(coordinates32).coordinates shouldBe Coordinates(2, 3)
      }
    }
  }
  "Upper Row, Left Column relative Node" when {
    "Coordinates are (3, 2)" should {
      "have Coordinates (2, 1)" in {
        UpperRowLeftColumnNode.from(coordinates32).coordinates shouldBe Coordinates(2, 1)
      }
    }
  }
}
