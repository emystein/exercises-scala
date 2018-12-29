package ar.com.flow.hitcounter

object EndpointMaxHitCounter extends App {
  def maxBetweenTwoEndpointHits(e1: EndpointHitCount, e2: EndpointHitCount): EndpointHitCount = if (e1.count > e2.count) e1 else e2

  def max(s: Iterable[EndpointHitCount]): Int = { s.reduceLeft(maxBetweenTwoEndpointHits).count }

  def process(log: Seq[String]) = {
    val counts = EndpointHitCounterByTimestamp.process(log)

    counts.map(count => (count._1, count._2.filter(_.count == max(count._2)).map(_.endpoint)))
  }

}
