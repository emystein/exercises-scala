package ar.com.flow.kata.pascaltriangle

import ar.com.flow.kata.pascaltriangle.PascalTriangle.topRow
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.TableDrivenPropertyChecks._

class NodeTest extends WordSpec with Matchers {
  "Left Parent, Right Parent" in {
    val table = Table(
      ("row", "column", "leftParent", "rightParent"),
      (topRow, 1, None, None),
      (topRow + 1, 1, None, Some(Node(topRow, 1))),
      (topRow + 1, 2, Some(Node(topRow, 1)), None),
      (topRow + 2, 1, None, Some(Node(topRow + 1, 1))),
      (topRow + 2, 2, Some(Node(topRow + 1, 1)), Some(Node(topRow + 1, 2))),
      (topRow + 2, 3, Some(Node(topRow + 1, 2)), None),
      (topRow + 3, 1, None, Some(Node(topRow + 2, 1))),
      (topRow + 3, 4, Some(Node(topRow + 2, 3)), None),
    )

    forAll(table) { (row: Int, column: Int, leftParent: Option[Node], rightParent: Option[Node]) =>
      val node = Node(row, column)
      node.leftParent shouldBe leftParent
      node.rightParent shouldBe rightParent
    }
  }
  "Parents" in {
    val table = Table(
      ("row", "column", "expected"),
      (topRow, 1, Nil),
      (topRow + 1, 1, List(Node(topRow, 1))),
      (topRow + 1, 2, List(Node(topRow, 1))),
      (topRow + 2, 2, List(Node(topRow + 1, 1), Node(topRow + 1, 2))),
    )

    forAll(table) { (row: Int, column: Int, expected: List[Node]) =>
      Node(row, column).parents shouldBe expected
    }
  }
  "Value" in {
    val table = Table(
      ("row", "column", "expected"),
      (topRow, 1, 1),
      (topRow + 1, 1, 1),
      (topRow + 1, 2, 1),
      (topRow + 2, 2, 2),
      (topRow + 3, 2, 3)
    )

    forAll(table) { (row: Int, column: Int, expected: Int) =>
      Node(row, column).value shouldBe expected
    }
  }
}
