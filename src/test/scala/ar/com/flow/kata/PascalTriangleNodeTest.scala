package ar.com.flow.kata

import ar.com.flow.kata.PascalTriangle.topRow
import org.scalatest.{Matchers, WordSpec}

class PascalTriangleNodeTest extends WordSpec with Matchers {
  "Node" when {
    "is top" should {
      val topNode = Node(row = topRow, column = 1)

      "return no parents" in {
        topNode.parents shouldBe(None, None)
      }
    }
    "is in second row first column" should {
      "return parents (None, (topRow, 1))" in {
        val node = Node(row = topRow + 1, column = 1)
        node.parents shouldBe(None, Some(Node(row = node.row - 1, column = 1)))
      }
    }
    "is in second row, second column" should {
      "return parents ((topRow, 1), None)" in {
        val node = Node(row = topRow + 1, column = 2)
        node.parents shouldBe(Some(Node(row = node.row - 1, column = 1)), None)
      }
    }
    "is in third row, first column" should {
      "return parents (None, (topRow + 1, 1))" in {
        val node = Node(row = topRow + 2, column = 1)
        node.parents shouldBe(None, Some(Node(row = node.row - 1, column = 1)))
      }
    }
    "is in third row, second column" should {
      "return parents ((topRow + 1, 1), (topRow + 1, 2)))" in {
        val node = Node(row = topRow + 2, column = 2)
        node.parents shouldBe(Some(Node(row = node.row - 1, column = 1)), Some(Node(row = node.row - 1, column = 2)))
      }
    }
    "is in third row, third column" should {
      "return parents ((topRow + 1, 2), None))" in {
        val node = Node(row = topRow + 2, column = 3)
        node.parents shouldBe(Some(Node(row = node.row - 1, column = 2)), None)
      }
    }
    "is in fourth row, first column" should {
      "return parents (None, (topRow + 2, 1))" in {
        val node = Node(row = topRow + 3, column = 1)
        node.parents shouldBe(None, Some(Node(row = node.row - 1, column = 1)))
      }
    }
    "is in fourth row, fourth column" should {
      "return parents ((topRow + 2, 3), None)" in {
        val node = Node(row = topRow + 3, column = 4)
        node.parents shouldBe(Some(Node(row = node.row - 1, column = 3)), None)
      }
    }
  }
  "Left Parent" when {
    "parents tuple has data" should {
      "be first component of parents tuple" in {
        val node = Node(row = topRow + 1, column = 1)

        node.parents shouldBe(None, Some(Node(row = topRow, column = 1)))

        node.leftParent shouldBe None
      }
    }
  }
  "Right Parent" when {
    "parents tupble has data" should {
      "be second component of parents tuple" in {
        val node = Node(row = topRow + 1, column = 1)

        node.parents shouldBe(None, Some(Node(row = topRow, column = 1)))

        node.rightParent shouldBe Some(Node(row = topRow, column = 1))
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

        topNode.presentParents shouldBe Nil
      }
    }
    "Node with no left parent and right parent" should {
      "have one present parent" in {
        val node = Node(row = topRow + 1, column = 1)

        node.presentParents shouldBe Seq(Node(node.row - 1, 1))
      }
    }
    "Node with left parent and no right parent" should {
      "have one present parent" in {
        val node = Node(row = topRow + 1, column = 2)

        node.presentParents shouldBe Seq(Node(node.row - 1, 1))
      }
    }
    "Node with both parents" should {
      "have two present parents" in {
        val node = Node(row = topRow + 2, column = 2)

        node.presentParents shouldBe Seq(Node(row = node.row - 1, column = 1), Node(row = node.row - 1, column = 2))
      }
    }
  }
}
