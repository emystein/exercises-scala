package ar.com.kamikazesoftware.hitcounter

import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

class HostAndPathJoinerTest extends FunSuite with BeforeAndAfter with Matchers {
  test("Join host and endpoint") {
    val logLine = "1129392323,127.0.0.1,/endpoint"

    val processed = HostAndPathJoiner.join(logLine)

    assert(processed == ("1129392323","127.0.0.1/endpoint"))
  }

}
