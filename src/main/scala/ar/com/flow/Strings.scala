package ar.com.flow

object Strings {
  def replaceSpaceWithUnderscore: String => String = { a: String => a.replaceAll(" ", "_") }

  def exclaimWords: String => String = { a: String => a.split(" ").filterNot(_.isEmpty).map(_ + "!").mkString(" ") }

  def toUpperCase: String => String = { a: String => a.toUpperCase }

  val concatenate: (String, String) => String = (s1: String, s2: String) => s1 + s2

  val ltrim = (s: String) => s.replaceAll("^\\s+", "")
  val rtrim = (s: String) => s.replaceAll("\\s+$", "")

  def splitByMiddleWord(string: String): (String, String, String) = {
    if (string.isEmpty) {
      ("", "", "")
    } else if (!string.contains(' ')) {
      ("", string, "")
    } else if (string.charAt(string.length / 2) == ' ') {
      val (left, right) = string.splitAt(string.length / 2)
      (left, "", right.drop(1))
    } else {
      val (left, right) = string.splitAt(string.length / 2)
      val leftMinusItsLastWord = left.take(left.lastIndexOf(' ') + 1)
      val rightMinusItsFirstWord = right.drop(right.indexOf(' '))
      val middleWord = string.substring(leftMinusItsLastWord.length, string.length - rightMinusItsFirstWord.length)
      (leftMinusItsLastWord, middleWord, rightMinusItsFirstWord)
    }
  }
}
