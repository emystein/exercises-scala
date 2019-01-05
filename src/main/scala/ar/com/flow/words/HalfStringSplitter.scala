package ar.com.flow.words

object HalfStringSplitter {
  def splitWithoutCuttingWords(string: String): (String, String) = {
    val (left, right) = string.splitAt(string.length / 2)
    val rightFirstSpaceIndex = if (right.contains(' ')) right.indexOf(' ') else right.length
    val rightSplitAtFirstSpace = right.splitAt(rightFirstSpaceIndex)
    (left + rightSplitAtFirstSpace._1, rightSplitAtFirstSpace._2)
  }
}
