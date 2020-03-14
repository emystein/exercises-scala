package ar.com.flow.fp.manning.chapter5

import ar.com.flow.fp.manning.chapter5
import org.scalatest.{FunSuite, Matchers}

class StreamTest extends FunSuite with Matchers {
  val stream123 = chapter5.Stream.cons(1, chapter5.Stream.cons(2, chapter5.Stream.cons(3, chapter5.Stream.empty)))
  
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  test("toList") {
    stream123.toList shouldBe List(1, 2, 3)
  }
  test("take 1") {
    stream123.take(1).toList shouldBe List(1)
  }
  test("take all") {
    stream123.take(3).toList shouldBe stream123.toList
  }
  test("drop 1") {
    stream123.drop(1).toList shouldBe List(2, 3)
  }
  test("drop all") {
    stream123.drop(3).toList shouldBe List.empty
  }
  test("headOption using foldRight") {
    stream123.headOptionUsingFoldRight shouldBe Some(1)
  }
  test("None headOption using foldRight") {
    chapter5.Stream.empty.headOptionUsingFoldRight shouldBe None
  }
  test("takeWhile") {
    stream123.takeWhile(_ != 2).toList shouldBe List(1)
  }
  test("forAll false") {
    stream123.forAll(_ == 1) shouldBe false
  }
  test("from 1") {
    stream123.from(1).take(3).toList shouldBe List(1, 2, 3)
  }
  // TODO
  test("fibs 0") {
    fibs.take(1).toList shouldBe List(0)
  }
  test("fibs 1") {
    fibs.take(2).toList shouldBe List(0, 1)
  }
  test("fibs 2") {
    fibs.take(3).toList shouldBe List(0, 1, 1)
  }
  test("fibs 3") {
    fibs.take(4).toList shouldBe List(0, 1, 1, 2)
  }
  test("fibs 4") {
    fibs.take(5).toList shouldBe List(0, 1, 1, 2, 3)
  }
}
