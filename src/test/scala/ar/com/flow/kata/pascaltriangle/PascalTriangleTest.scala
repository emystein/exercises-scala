package ar.com.flow.kata.pascaltriangle

import org.scalatest.{FlatSpec, Matchers}

class PascalTriangleTest extends FlatSpec with Matchers {
  it should "return no parents of top node" in {
    val topNode = Node(row = topRow, column = 1)
    topNode.parents shouldBe(None, None)
  }
  it should "return parents (None, (topRow, 1)) of node (2, 1)" in {
    val node = Node(row = topRow + 1, column = 1)
    node.parents shouldBe(None, Some(Node(row = node.row - 1, column = 1)))
  }
  it should "return parents ((topRow, 1), None) of node (topRow + 1, 2)" in {
    val node = Node(row = topRow + 1, column = 2)
    node.parents shouldBe(Some(Node(row = node.row - 1, column = 1)), None)
  }
  it should "return parents (None, (topRow + 1, 1)) of node (topRow + 2, 1)" in {
    val node = Node(row = topRow + 2, column = 1)
    node.parents shouldBe(None, Some(Node(row = node.row - 1, column = 1)))
  }
  it should "return parents ((topRow + 1, 1), (topRow + 1, 2)) of node (topRow + 2, 2)" in {
    val node = Node(row = topRow + 2, column = 2)
    node.parents shouldBe(Some(Node(row = node.row - 1, column = 1)), Some(Node(row = node.row - 1, column = 2)))
  }
  it should "return parents ((topRow + 1, 2), None)) of node (topRow + 2, 3)" in {
    val node = Node(row = topRow + 2, column = 3)
    node.parents shouldBe(Some(Node(row = node.row - 1, column = 2)), None)
  }
  it should "return parents (None, (topRow + 2, 1)) of node (topRow + 3, 1)" in {
    val node = Node(row = topRow + 3, column = 1)
    node.parents shouldBe(None, Some(Node(row = node.row - 1, column = 1)))
  }
  it should "return parents ((topRow + 2, 3), None) of node (topRow + 3, 4)" in {
    val node = Node(row = topRow + 3, column = 4)
    node.parents shouldBe(Some(Node(row = node.row - 1, column = 3)), None)
  }
  it should "left parent should be first component of parents tuple" in {
    val node = Node(row = topRow + 1, column = 1)

    node.parents shouldBe(None, Some(Node(row = topRow, column = 1)))

    node.leftParent shouldBe None
  }
  it should "right parent should be second component of parents tuple" in {
    val node = Node(row = topRow + 1, column = 1)

    node.parents shouldBe(None, Some(Node(row = topRow, column = 1)))

    node.rightParent shouldBe Some(Node(row = topRow, column = 1))
  }
  it should "give value 1 to top node" in {
    val topNode = Node(row = topRow, column = 1)

    topNode.value shouldBe 1
  }
  it should "give value 1 to node with no left parent" in {
    val node = Node(row = topRow + 1, column = 1)

    node.value shouldBe 1
  }
  it should "give value 1 to node with no right parent" in {
    val node = Node(row = topRow + 1, column = 2)

    node.value shouldBe 1
  }
  it should "give value 2 to node (3, 2)" in {
    val node = Node(row = topRow + 2, column = 2)

    node.value shouldBe 2
  }
  it should "give value 3 to node (topRow + 3, 2)" in {
    val node = Node(row = topRow + 3, column = 2)

    node.value shouldBe 3
  }
  it should "have no present parents on top node" in {
    val topNode = Node(row = topRow, column = 1)

    topNode.presentParents shouldBe Nil
  }
  it should "have one present parent on node with no left parent and right parent" in {
    val node = Node(row = topRow + 1, column = 1)

    node.presentParents shouldBe Seq(Node(node.row - 1, 1))
  }
  it should "have one present parent on node with left parent and no right parent" in {
    val node = Node(row = topRow + 1, column = 2)

    node.presentParents shouldBe Seq(Node(node.row - 1, 1))
  }
  it should "have two present parents in node with both parents" in {
    val node = Node(row = topRow + 2, column = 2)

    node.presentParents shouldBe Seq(Node(row = node.row - 1, column = 1), Node(row = node.row - 1, column = 2))
  }
}




