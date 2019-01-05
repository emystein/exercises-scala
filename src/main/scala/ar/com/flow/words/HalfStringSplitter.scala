package ar.com.flow.words

object HalfStringSplitter {
  def splitWithoutCuttingWords(string: String): (String, String) = {
    val (left, right) = string.splitAt(string.length / 2)

    val rightFirstSpaceIndex = right.indexOf(' ')

    if (rightFirstSpaceIndex >= 0) {
      val rightSplitAtFirstSpace = right.splitAt(rightFirstSpaceIndex)
      (left + rightSplitAtFirstSpace._1, rightSplitAtFirstSpace._2.drop(1))
    } else {
      (left + right, "")
    }
  }
}
