package ar.com.flow.kata.pascaltriangle

import ar.com.flow.kata.pascaltriangle.PascalTriangle.topRow
import ar.com.flow.kata.pascaltriangle.PositionRelativeToEdges.{Internal, LeftEdge, RightEdge, Top}
import org.scalatest.{Matchers, WordSpec}

class PascalTriangleCoordinatesTest extends WordSpec with Matchers {
  "Coordinates" when {
    "Row 1, Column 1" should {
      "have upperRow equal to 0" in {
        Coordinates(1, 1).upperRow shouldBe 0
      }
      "have leftColumn equal to 0" in {
        Coordinates(1, 1).leftColumn shouldBe 0
      }
    }
  }
  "PositionRelativeToEdges" when {
    "Coordinates are (topRow, 1)" should {
      "be Top" in {
        PositionRelativeToEdges.of(Coordinates(topRow, 1)) shouldBe Top()
      }
    }
    "Coordinates are (topRow + 1, 1)" should {
      "be LeftEdge" in {
        PositionRelativeToEdges.of(Coordinates(topRow + 1, 1)) shouldBe LeftEdge()
      }
    }
    "Coordinates are (topRow + 1, 2)" should {
      "be RightEdge" in {
        PositionRelativeToEdges.of(Coordinates(topRow + 1, 2)) shouldBe RightEdge()
      }
    }
    "Coordinates are (topRow + 2, 1)" should {
      "be LeftEdge" in {
        PositionRelativeToEdges.of(Coordinates(topRow + 2, 1)) shouldBe LeftEdge()
      }
    }
    "Coordinates are (topRow + 2, 2)" should {
      "be Inside" in {
        PositionRelativeToEdges.of(Coordinates(topRow + 2, 2)) shouldBe Internal()
      }
    }
    "Coordinates are (topRow + 2, 3)" should {
      "be RightEdge" in {
        PositionRelativeToEdges.of(Coordinates(topRow + 2, 3)) shouldBe RightEdge()
      }
    }
  }
}
