package ar.com.flow.kata

import org.scalatest._
import org.scalatest.Assertions._
import ThirteenTest._

class ThirteenTest extends FlatSpec { 
  it should "pass basic tests" in {
    dotest(1234567, 87)
    dotest(8529, 79)
    dotest(85299258, 31)
    dotest(5634, 57)
    dotest(1111111111, 71)
  }
}

object ThirteenTest {
  private def dotest(n: Long, expect: Long): Unit = {
    println("Testing: " + n)
    val actual: Long = Thirteen.thirt(n)
    assertResult(expect){actual}
  }
}