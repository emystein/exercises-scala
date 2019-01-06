package ar.com.flow.words

object HalfStringSplitter {
  def splitWithoutCuttingWords(string: String): (String, String) = {
    val rightHalf = string.drop(string.length / 2)

    if (rightHalf.contains(' ')) {
      string.splitAt((string.length / 2) + rightHalf.indexOf(' '))
    } else {
      (string, "")
    }
  }
}
