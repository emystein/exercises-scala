package ar.com.flow.kata

import org.scalatest.{FlatSpec, Matchers}

class EnglishBeggarsKataTest extends FlatSpec with Matchers {
  val tests = List(
    ((List(1, 2, 3, 4, 5), 1) -> List(15)),
    ((List(1, 2, 3, 4, 5), 2) -> List(9,6)),
    ((List(1, 2, 3, 4, 5), 3) -> List(5,7,3)),
    ((List(1, 2, 3, 4, 5), 6) -> List(1,2,3,4,5,0)),
    ((List(1, 2, 3, 4, 5), 0) -> List())
  )

  tests.foreach {
    case ((values, n), expected) =>
      s"beggars($values, $n)" should s"return $expected" in {
        EnglishBeggars.beggars(values, n) should be (expected)
      }
  }
}
