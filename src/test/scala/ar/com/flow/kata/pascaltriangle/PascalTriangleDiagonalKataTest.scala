package ar.com.flow.kata.pascaltriangle

import org.scalatest.{FlatSpec, Matchers}

class PascalTriangleDiagonalKataTest extends FlatSpec with Matchers {
  it should "pass basic tests" in {
    testing(20, 3, 5985)
    testing(20, 4, 20349)
    testing(20, 5, 54264)
    testing(20, 15, 20349)
  }

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
