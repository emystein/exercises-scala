package ar.com.flow.words

object HalfStringSplitter {
  def splitWithoutCuttingWords(string: String): (String, String) = {
    val (leftHalf, rightHalf) = string.splitAt(string.length / 2)
    val rightFirstSpaceIndex = if (rightHalf.contains(' ')) rightHalf.indexOf(' ') else rightHalf.length
    val splitRightAtFirstSpace = rightHalf.splitAt(rightFirstSpaceIndex)

    (leftHalf + splitRightAtFirstSpace._1, splitRightAtFirstSpace._2)
  }
}
