package ar.com.flow.kata.pascaltriangle

import ar.com.flow.kata.pascaltriangle.PascalTriangle.topRow
import ar.com.flow.kata.pascaltriangle.PositionRelativeToEdges.{Internal, LeftEdge, RightEdge, Top}
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{Matchers, WordSpec}

class CoordinatesTest extends WordSpec with Matchers {
  "Coordinates" in {
    Coordinates(1, 1).upperRow shouldBe 0
    Coordinates(1, 1).leftColumn shouldBe 0
  }
  "PositionRelativeToEdges" in {
    val positions = Table(
      ("row", "column", "expected"),
      (topRow, 1, Top()),
      (topRow + 1, 1, LeftEdge()),
      (topRow + 1, 2, RightEdge()),
      (topRow + 2, 1, LeftEdge()),
      (topRow + 2, 2, Internal()),
      (topRow + 2, 3, RightEdge()),
    )

    forAll(positions) { (row: Int, column: Int, expected: PositionRelativeToEdges) =>
      PositionRelativeToEdges.of(Coordinates(row, column)) shouldBe expected
    }
  }
}
