package ar.com.flow.strings

object HalfStringSplitter {
  def splitWithoutCuttingWords(string: String): (String, String) = {
    val rightHalf = string.drop(string.length / 2)

    if (rightHalf.contains(' ')) {
      val (resultLeft, resultRight) = string.splitAt((string.length / 2) + rightHalf.indexOf(' '))
      (resultLeft, resultRight)
    } else {
      (string, "")
    }
  }
}
