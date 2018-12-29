package ar.com.kamikazesoftware.hitcounter

import org.scalatest.{AsyncFunSuite, FunSuite, Matchers}

import scala.concurrent.Future

class FutureForComprehensionTest extends AsyncFunSuite with Matchers {
  test("Future of multiply by 2") {
    val source = Future.successful(Seq(1, 2, 3))

    val result = for {
      number <- source
    } yield for {
      num <- number
    } yield num * 2

    result.map({r => r should contain allElementsOf Seq(2, 4, 6)})
  }

}
