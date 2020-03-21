package ar.com.flow.kata.pascaltriangle

import ar.com.flow.kata.pascaltriangle.PascalTriangle.topRow
import org.scalatest.{Matchers, WordSpec}

class NodeTest extends WordSpec with Matchers {
  "Node" when {
    "is top" should {
      val topNode = Node(row = topRow, column = 1)

      "return no parents" in {
        topNode.leftParent shouldBe None
        topNode.rightParent shouldBe None
      }
    }
    "is in second row first column" should {
      "return parents (topRow, 1)" in {
        val node = Node(row = topRow + 1, column = 1)

        node.leftParent shouldBe None
        node.rightParent shouldBe Some(Node(row = node.coordinates.upperRow, column = 1))
      }
    }
    "is in second row, second column" should {
      "return parents (topRow, 1)" in {
        val node = Node(row = topRow + 1, column = 2)

        node.leftParent shouldBe Some(Node(row = node.coordinates.upperRow, column = 1))
        node.rightParent shouldBe None
      }
    }
    "is in third row, first column" should {
      "return parents (topRow + 1, 1)" in {
        val node = Node(row = topRow + 2, column = 1)

        node.leftParent shouldBe None
        node.rightParent shouldBe Some(Node(row = node.coordinates.upperRow, column = 1))
      }
    }
    "is in third row, second column" should {
      "return parents (topRow + 1, 1), (topRow + 1, 2)" in {
        val node = Node(row = topRow + 2, column = 2)

        node.leftParent shouldBe Some(Node(row = node.coordinates.upperRow, column = 1))
        node.rightParent shouldBe Some(Node(row = node.coordinates.upperRow, column = 2))
      }
    }
    "is in third row, third column" should {
      "return parents (topRow + 1, 2)" in {
        val node = Node(row = topRow + 2, column = 3)

        node.leftParent shouldBe Some(Node(row = node.coordinates.upperRow, column = 2))
        node.rightParent shouldBe None
      }
    }
    "is in fourth row, first column" should {
      "return parents (topRow + 2, 1)" in {
        val node = Node(row = topRow + 3, column = 1)

        node.leftParent shouldBe None
        node.rightParent shouldBe Some(Node(row = node.coordinates.upperRow, column = 1))
      }
    }
    "is in fourth row, fourth column" should {
      "return parents (topRow + 2, 3)" in {
        val node = Node(row = topRow + 3, column = 4)

        node.leftParent shouldBe Some(Node(row = node.coordinates.upperRow, column = 3))
        node.rightParent shouldBe None
      }
    }
  }
  "Value" when {
    "Top Node" should {
      "be 1" in {
        val topNode = Node(row = topRow, column = 1)

        topNode.value shouldBe 1
      }
    }
    "Node has no left parent" should {
      "be 1" in {
        val node = Node(row = topRow + 1, column = 1)

        node.value shouldBe 1
      }
    }
    "Node has no right parent" should {
      "be 1" in {
        val node = Node(row = topRow + 1, column = 2)

        node.value shouldBe 1
      }
    }
    "Node (topRow + 2, 1)" should {
      "be 2" in {
        val node = Node(row = topRow + 2, column = 2)

        node.value shouldBe 2
      }
    }
    "Node (topRow + 3, 2)" should {
      "be 3" in {
        val node = Node(row = topRow + 3, column = 2)

        node.value shouldBe 3
      }
    }
  }
  "Present Parents" when {
    "Top Node" should {
      "have no present parents" in {
        val topNode = Node(row = topRow, column = 1)

        topNode.parents shouldBe Nil
      }
    }
    "Node with no left parent and right parent" should {
      "have one present parent" in {
        val node = Node(row = topRow + 1, column = 1)

        node.parents shouldBe Seq(Node(node.coordinates.upperRow, 1))
      }
    }
    "Node with left parent and no right parent" should {
      "have one present parent" in {
        val node = Node(row = topRow + 1, column = 2)

        node.parents shouldBe Seq(Node(node.coordinates.upperRow, 1))
      }
    }
    "Node with both parents" should {
      "have two present parents" in {
        val node = Node(row = topRow + 2, column = 2)

        node.parents shouldBe Seq(Node(row = node.coordinates.upperRow, column = 1), Node(row = node.coordinates.upperRow, column = 2))
      }
    }
  }
}
