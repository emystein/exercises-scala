package ar.com.kamikazesoftware.hitcounter

object EndpointHitCounterByTimestamp {
  def process(log: Seq[String]) : Map[String, Iterable[EndpointHitCount]] = {
    log.groupBy(line => line.split(",").head)
      .map({case (timestamp, endpoints) =>
        (timestamp -> endpoints.map(hostAndPath => hostAndPath.split(",").tail.mkString)
          .groupBy(identity).mapValues(_.size).map(e => EndpointHitCount(e._1, e._2)))})
  }
}
