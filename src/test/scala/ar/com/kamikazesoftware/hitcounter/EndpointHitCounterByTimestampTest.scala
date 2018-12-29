package ar.com.kamikazesoftware.hitcounter

import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

class EndpointHitCounterByTimestampTest extends FunSuite with BeforeAndAfter with Matchers {
  var log : Seq[String] = Seq()

  before {
    log = Seq("1129392323,127.0.0.1,/endpoint",
              "1129392323,127.0.0.1,/endpoint",
              "1129392323,127.0.0.1,/anotherEndpoint",
              "1129392324,127.0.0.1,/anotherEndpoint",
              "1129392325,127.0.0.1,/endpoint")
  }

  test("Count endpoint hits by timestamp") {
    val result = EndpointHitCounterByTimestamp.process(log)

    result("1129392323") should contain allElementsOf Set(EndpointHitCount("127.0.0.1/endpoint", 2), EndpointHitCount("127.0.0.1/anotherEndpoint", 1))
  }

}
