package ar.com.flow.fp.manning.chapter5

import ar.com.flow.fp.manning.chapter5
import org.scalatest.{FunSuite, Matchers}

class StreamTest extends FunSuite with Matchers {
  val stream12 = chapter5.Stream.cons(1, chapter5.Stream.cons(2, chapter5.Stream.empty))

  test("toList") {
    stream12.toList shouldBe List(1, 2)
  }
  test("take 1") {
    stream12.take(1).toList shouldBe List(1)
  }
  test("take all") {
    stream12.take(2).toList shouldBe stream12.toList
  }
  test("drop 1") {
    stream12.drop(1).toList shouldBe List(2)
  }
  test("drop all") {
    stream12.drop(2).toList shouldBe List.empty
  }
  test("headOption using foldRight") {
    stream12.headOptionUsingFoldRight shouldBe Some(1)
  }
  test("None headOption using foldRight") {
    chapter5.Stream.empty.headOptionUsingFoldRight shouldBe None
  }
  test("takeWhile") {
    stream12.takeWhile(_ != 1).toList shouldBe List(2)
  }
}
