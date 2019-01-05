package ar.com.flow.words

object HalfStringSplitter {
  def splitWithoutCuttingWords(string: String): (String, String) = {
    val (l, r) = string.splitAt(string.length / 2)

    val left = l + (if (r.contains(' ')) r.substring(0, r.indexOf(' ')) else r)
    val right = if (r.contains(' ')) r.substring(r.indexOf(' ') + 1) else ""

    (left, right)
  }
}
