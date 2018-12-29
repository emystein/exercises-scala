package ar.com.flow.hitcounter

import org.scalatest.{FunSuite, Matchers}

class EndpointHitsGroupedByTimestampTest extends FunSuite with Matchers {

  test("Log with a single entry") {
    val log = Seq("1129392323,127.0.0.1,/endpoint")

    val result = EndpointHitsGroupedByTimestamp.process(log)

    result("1129392323") shouldBe Seq("127.0.0.1/endpoint")
  }

  test("Two hits to different endpoints, at the same time") {
    val log = Seq("1129392323,127.0.0.1,/endpoint1", "1129392323,127.0.0.1,/endpoint2")

    val result = EndpointHitsGroupedByTimestamp.process(log)

    result("1129392323") shouldBe Seq("127.0.0.1/endpoint1", "127.0.0.1/endpoint2")
  }

  test("Two hits to the same endpoint, at the same time") {
    val log = Seq("1129392323,127.0.0.1,/endpoint1", "1129392323,127.0.0.1,/endpoint1")

    val result = EndpointHitsGroupedByTimestamp.process(log)

    result("1129392323") shouldBe Seq("127.0.0.1/endpoint1", "127.0.0.1/endpoint1")
  }

}
