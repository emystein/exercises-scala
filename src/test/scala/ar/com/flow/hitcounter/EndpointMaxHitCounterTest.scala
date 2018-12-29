package ar.com.flow.hitcounter

import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

/**

You have a log file that shows hits per second with the following format:

"1129392323", "127.0.0.1", "/endpoint"
"1129392324", "127.0.0.1", "/anotherEndpoint"
"1129392325", "127.0.0.1", "/endpoint"

Assume you receive the log file already parsed in a List<String[]> structure.
Implement a method that returns for every given second in the logfile, the endpoints with more hits.
  
  */
class EndpointMaxHitCounterTest extends FunSuite with BeforeAndAfter with Matchers {
  var log : Seq[String] = Seq()

  before {
    log = Seq("1129392323,127.0.0.1,/endpoint",
              "1129392323,127.0.0.1,/endpoint",
              "1129392323,127.0.0.1,/anotherEndpoint",
              "1129392324,127.0.0.1,/endpoint",
              "1129392324,127.0.0.1,/anotherEndpoint")
  }

  test("Count endpoint hits") {
    val result = EndpointMaxHitCounter.process(log)

    result("1129392323") should have size 1
    result("1129392323") should contain allElementsOf Set("127.0.0.1/endpoint")

    result("1129392324") should have size 2
    result("1129392324") should contain allElementsOf Set("127.0.0.1/endpoint","127.0.0.1/anotherEndpoint")
  }
}
