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
  test("ones in terms of unfold") {
    val ones = Stream.unfold(1)(s => Some((s, s)))

    ones.take(3).toList shouldBe List(1, 1, 1)
  }
  test("constant in terms of unfold") {
    def constant(n: Int) = Stream.unfold(n)(s => Some((s, s)))

    constant(2).take(3).toList shouldBe List(2, 2, 2)
  }
  test("from in terms of unfold") {
    def from(n: Int) = Stream.unfold(n)(s => Some((s, s + 1)))

    from(1).take(3).toList shouldBe List(1, 2, 3)
  }
  test("fibs in terms of unfold") {
    val fibs = Stream.unfold((0, 1)){case (a, b) => Some((a, (b, (a + b))))}

    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }
}
