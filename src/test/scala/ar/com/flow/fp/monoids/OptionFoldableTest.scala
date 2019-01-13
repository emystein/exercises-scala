package ar.com.flow.fp.monoids

import org.scalatest.{FunSuite, Matchers}

class OptionFoldableTest extends FunSuite with Matchers {
  val optionFoldable = new OptionFoldable

  test("Fold None") {
    val sumOption = (a: Int, b: Option[Int]) => a + b.getOrElse(0)

    optionFoldable.foldLeft(None)(0)(sumOption) shouldBe 0
  }

  test("Fold Some number") {
    optionFoldable.foldLeft(Some(1))(1)(_ + _) shouldBe 2
  }
}
