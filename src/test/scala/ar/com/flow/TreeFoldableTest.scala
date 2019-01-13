package ar.com.flow

import org.scalatest.{FunSuite, Matchers}

class TreeFoldableTest extends FunSuite with Matchers {
  val foldable = new TreeFoldable

  test("Fold a Leaf") {
    val leaf = Leaf("a")

    foldable.foldLeft(leaf)(0)(_ + _.length) shouldBe 1
  }

  test("Fold a Branch with left Leaf and right Leaf") {
    val leftLeaf = Leaf("a")
    val rightLeaf = Leaf("b")
    val tree = Branch(leftLeaf, rightLeaf)

    foldable.foldLeft(tree)(0)(_ + _.length) shouldBe 2
  }

  test("Fold a Branch with left Leaf and right Branch with leafs") {
    val left = Leaf("a")
    val right = Branch(Leaf("b"), Leaf("c"))
    val tree = Branch(left, right)

    foldable.foldLeft(tree)(0)(_ + _.length) shouldBe 3
  }

  test("Fold a Branch with left Branch with leafs and right Leaf") {
    val left = Branch(Leaf("a"), Leaf("b"))
    val right = Leaf("c")
    val tree = Branch(left, right)

    foldable.foldLeft(tree)(0)(_ + _.length) shouldBe 3
  }

  test("Fold a Branch with left Branch with leafs and right Branch with leafs") {
    val left = Branch(Leaf("a"), Leaf("b"))
    val right = Branch(Leaf("c"), Leaf("d"))
    val tree = Branch(left, right)

    foldable.foldLeft(tree)(0)(_ + _.length) shouldBe 4
  }

  test("Fold a Branch with left Branch with Branches and right Branch with Branches") {
    val leftLeft = Branch(Leaf("a"), Leaf("b"))
    val leftRight = Branch(Leaf("c"), Leaf("d"))
    val rightLeft = Branch(Leaf("e"), Leaf("f"))
    val rightRight = Branch(Leaf("g"), Leaf("h"))
    val left = Branch(leftLeft, leftRight)
    val right = Branch(rightLeft, rightRight)
    val tree = Branch(left, right)

    foldable.foldLeft(tree)("")(_ + _) shouldBe "abcdefgh"
  }
}
