package ar.com.flow.words

object HalfStringSplitter {
  def splitWithoutCuttingWords(string: String): (String, String) = {
    val rightHalf = string.drop(string.length / 2)
    val rightFirstSpaceIndex = if (rightHalf.contains(' ')) rightHalf.indexOf(' ') else rightHalf.length
    val halfIndexWithoutCuttingWords = (string.length / 2) + rightFirstSpaceIndex
    string.splitAt(halfIndexWithoutCuttingWords)
  }
}
