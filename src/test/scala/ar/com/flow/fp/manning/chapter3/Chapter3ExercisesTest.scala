package ar.com.flow.fp.manning.chapter3

import Chapter3Exercises._
import org.scalatest.{FunSuite, Matchers}

class Chapter3ExercisesTest extends FunSuite with Matchers {
  val list = List(1, 2, 3)

  test("foldRight") {
    foldRight(list, 1)(_ * _) shouldBe 6
  }

  test("length using foldRight") {
    lengthUsingFoldRight(Nil) shouldBe 0
    lengthUsingFoldRight(List("a")) shouldBe 1
    lengthUsingFoldRight(List("a", "b")) shouldBe 2
    lengthUsingFoldRight(list) shouldBe 3
  }

  test("reverse using foldRight") {
    reverseUsingFoldRight(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  test("foldLeft") {
    foldLeft(list, 6)(_ / _) shouldBe 1
  }

  test("sum using foldLeft") {
    sumUsingFoldLeft(List[Long](1, 2, 3)) shouldBe 6
  }

  test("product using foldLeft") {
    sumUsingFoldLeft(List(1, 2, 3)) shouldBe 6
  }

  test("length using foldLeft") {
    lengthUsingFoldLeft(List(1, 2, 3)) shouldBe 3
  }

  test("append using fold") {
    appendUsingFold(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  test("last using foldLeft") {
    foldLeft(list, list.head)((_, x) => x) shouldBe 3
  }

  test("reverse using foldLeft") {
    foldLeft(list, List[Int]())((xs, x) => x :: xs) shouldBe List(3, 2, 1)
  }

  // TODO fix
//  test("custom reverse") {
//    reverse(list) shouldBe List(3, 2, 1)
//  }

  test("append element to list") {
    appendElement(list, 4) shouldBe List(1, 2, 3, 4)
  }

  test("last element of list") {
    last(list) shouldBe 3
  }

  // TODO
  //  test("foldLeft in terms of foldRight") {
  //    foldLeftInTermsOfFoldRight(list, 6)(_ / _) shouldBe 1
  //  }

  test("add 1 to every element of a list") {
    add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  test("convert List of Double into List of String") {
    convertListOfDoubleToListOfString(List(1D, 2D, 3D)) shouldBe List("1.0", "2.0", "3.0")
  }

  test("custom map") {
    map(List(1, 2, 3))(x => x + 1) shouldBe List(2, 3, 4)
  }

  test("custom filter") {
    filter(List(1, 2, 3))(x => x % 2 == 0) shouldBe List(2)
  }

  test("custom flatMap") {
    flatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  test("custom filter using custom flatMap") {
    filterUsingFlatMap(List(1, 2, 3))(x => x % 2 == 0) shouldBe List(2)
  }

  test("sum two lists element by element") {
    sumMerge(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }

  test("custom zipWith") {
    zipWith(List(1, 2, 3), List(4, 5, 6))((x, y) => x + y) shouldBe List(5, 7, 9)
  }

  test("hasSubsequence") {
    hasSubsequence(List(1, 2, 3), List(1)) shouldBe true
  }
}
