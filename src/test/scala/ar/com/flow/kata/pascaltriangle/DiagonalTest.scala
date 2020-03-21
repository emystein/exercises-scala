package ar.com.flow.kata.pascaltriangle

import ar.com.flow.kata.pascaltriangle.PascalTriangle.topRow
import org.scalatest._

class DiagonalTest extends WordSpec with Matchers {
  "Diagonal 0" when {
    val topRowDiagonal0 = Diagonal(row = topRow, diagonal = 0)
    "Row is topRow" should {
      "be Top Node" in {
        topRowDiagonal0.nodes shouldBe List(Node(row = topRow, column = 1))
      }
      "have sum 1" in {
        topRowDiagonal0.sum shouldBe 1
      }
    }
    "Row is 1" should {
      val row1Diagonal0 = Diagonal(row = topRow + 1, diagonal = 0)
      "be (topRow + 1, 1), (topRow, 1)" in {
        row1Diagonal0.nodes shouldBe List(Node(row = topRow + 1, column = 1), Node(row = topRow, column = 1))
      }
      "have sum 2" in {
        row1Diagonal0.sum shouldBe 2
      }
    }
    "Row is 2" should {
      val row2Diagonal0 = Diagonal(row = topRow + 2, diagonal = 0)
      "be (topRow + 2, 1), (topRow + 1, 1), (topRow, 1)" in {
        row2Diagonal0.nodes shouldBe List(Node(row = topRow + 2, column = 1), Node(row = topRow + 1, column = 1), Node(row = topRow, column = 1))
      }
      "have sum 3" in {
        row2Diagonal0.sum shouldBe 3
      }
    }
    "Row is 3" should {
      val row3Diagonal0 = Diagonal(row = topRow + 3, diagonal = 0)
      "be (topRow + 3, 1), (topRow + 2, 1), (topRow + 1, 1), (topRow, 1)" in {
        row3Diagonal0.nodes shouldBe List(Node(row = topRow + 3, column = 1), Node(row = topRow + 2, column = 1), Node(row = topRow + 1, column = 1), Node(row = topRow, column = 1))
      }
      "have sum 4" in {
        row3Diagonal0.sum shouldBe 4
      }
    }
  }
  "Diagonal 1" when {
    val row2Column2Node = Node(row = topRow + 1, column = 2)
    val row3Column2Node = Node(row = topRow + 2, column = 2)
    val row4Column2Node = Node(row = topRow + 3, column = 2)
    "Row is 1" should {
      "be (topRow + 1, 2)" in {
        Diagonal(row = topRow + 1, diagonal = 1).nodes shouldBe List(row2Column2Node)
      }
    }
    "Row is 2" should {
      "be (topRow + 2, 2), (topRow + 1, 2)" in {
        Diagonal(row = topRow + 2, diagonal = 1).nodes shouldBe List(row3Column2Node, row2Column2Node)
      }
    }
    "Row is 3" should {
      "be (topRow + 3, 2), (topRow + 2, 2), (topRow + 1, 2)" in {
        Diagonal(row = topRow + 3, diagonal = 1).nodes shouldBe List(row4Column2Node, row3Column2Node, row2Column2Node)
      }
    }
  }
  "Diagonal 2" when {
    val row3Column3Node = Node(row = topRow + 2, column = 3)
    val row4Column3Node = Node(row = topRow + 3, column = 3)
    "Row is 3" should {
      "be (topRow + 2, 3)" in {
        Diagonal(row = topRow + 2, diagonal = 2).nodes shouldBe List(row3Column3Node)
      }
    }
    "Row is 4" should {
      "be (topRow + 3, 3), (topRow + 2, 3)" in {
        Diagonal(row = topRow + 3, diagonal = 2).nodes shouldBe List(row4Column3Node, row3Column3Node)
      }
    }
  }
  "Diagonal sum" when {
    "Row is 20, Diagonal is 3" should {
      "be 5985" in {
        Diagonal(row = 20, diagonal = 3).sum shouldBe 5985
      }
    }
    "Row is 20, Diagonal is 4" should {
      "be 20349" in {
        Diagonal(row = 20, diagonal = 4).sum shouldBe 20349
      }
    }
    "Row is 20, Diagonal is 5" should {
      "be 54264" in {
        Diagonal(row = 20, diagonal = 5).sum shouldBe 54264
      }
    }
    "Row is 20, Diagonal is 15" should {
      "be 20349" in {
        Diagonal(row = 20, diagonal = 15).sum shouldBe 20349
      }
    }
  }
}

