package ar.com.flow.hitcounter

object EndpointHitsGroupedByTimestamp {
  def process(log: Seq[String]) = {
    log.groupBy(line => line.split(",").head)
       .map({case (timestamp, endpoints) => (timestamp -> endpoints.map(hostAndPath => hostAndPath.split(",").tail.mkString))})
  }

}
