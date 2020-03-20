package ar.com.flow.kata

import ar.com.flow.kata.PascalTriangleDiagonalKataTest.testing
import org.scalatest.Assertions.assertResult
import org.scalatest.{FlatSpec, Matchers}

class PascalTriangleDiagonalKataTest extends FlatSpec with Matchers {
  it should "pass basic tests" in {
    testing(20, 3, 5985)
    testing(20, 4, 20349)
    testing(20, 5, 54264)
    testing(20, 15, 20349)
  }
}

object PascalTriangleDiagonalKataTest {
  private def testing(n: Int, p: Int, expect: BigInt): Unit = {
    println("Testing: " + n + ", " + p)
    val actual: BigInt = Diagonal.diagonal(n, p)
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("*")
    assertResult(expect) {
      actual
    }
  }
}
