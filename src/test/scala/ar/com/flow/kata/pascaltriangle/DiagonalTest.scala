package ar.com.flow.kata.pascaltriangle

import ar.com.flow.kata.pascaltriangle.PascalTriangle.topRow
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

class DiagonalTest extends WordSpec with Matchers {
  "Diagonal nodes" in {
    val row1Column1Node = Node(row = topRow, column = 1)
    val row2Column1Node = Node(row = topRow + 1, column = 1)
    val row2Column2Node = Node(row = topRow + 1, column = 2)
    val row3Column1Node = Node(row = topRow + 2, column = 1)
    val row3Column2Node = Node(row = topRow + 2, column = 2)
    val row3Column3Node = Node(row = topRow + 2, column = 3)
    val row4Column1Node = Node(row = topRow + 3, column = 1)
    val row4Column2Node = Node(row = topRow + 3, column = 2)
    val row4Column3Node = Node(row = topRow + 3, column = 3)

    val diagonals = Table(
      ("row", "diagonal", "nodes"),
      (topRow, 0, List(row1Column1Node)),
      (topRow + 1, 0, List(row2Column1Node, row1Column1Node)),
      (topRow + 2, 0, List(row3Column1Node, row2Column1Node, row1Column1Node)),
      (topRow + 3, 0, List(row4Column1Node, row3Column1Node, row2Column1Node, row1Column1Node)),
      (topRow + 1, 1, List(row2Column2Node)),
      (topRow + 2, 1, List(row3Column2Node, row2Column2Node)),
      (topRow + 3, 1, List(row4Column2Node, row3Column2Node, row2Column2Node)),
      (topRow + 2, 2, List(row3Column3Node)),
      (topRow + 3, 2, List(row4Column3Node, row3Column3Node)),
    )

    forAll(diagonals) { (row: Int, diagonal: Int, nodes: List[Node]) =>
      Diagonal(row = row, diagonal = diagonal).nodes shouldBe nodes
    }
  }

  "Diagonal sum" in {
    val diagonals = Table(
      ("row", "diagonal", "sum"),
      (topRow, 0, 1),
      (topRow + 1, 0, 2),
      (topRow + 2, 0, 3),
      (topRow + 3, 0, 4),
      (20, 3, 5985),
      (20, 4, 20349),
      (20, 5, 54264),
      (20, 15, 20349)
    )

    forAll(diagonals) { (row: Int, diagonal: Int, sum: Int) =>
      Diagonal(row = row, diagonal = diagonal).sum shouldBe sum
    }
  }
}

