package ar.com.kamikazesoftware.hitcounter

object HostAndPathJoiner {
  def join(logLine: String) : (String, String) = {
    val split = logLine.split(",")
    (split(0), split(1) ++ split(2))
  }
}
