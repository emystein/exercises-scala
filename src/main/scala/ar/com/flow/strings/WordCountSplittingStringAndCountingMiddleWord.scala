package ar.com.flow.strings

import ar.com.flow.Strings
import ar.com.flow.Strings._
import ar.com.flow.BooleanImplicits._

object WordCountSplittingStringAndCountingMiddleWord {
  def apply(input: String): Int = {
    val string = ltrim(rtrim(input))

    if (string.isEmpty) {
      0
    } else if (!string.contains(' ')) {
      1
    } else {
      val (leftMinusItsLastWord, middleWord, rightMinusItsFirstWord) = Strings.splitByMiddleWord(string)

      bool2int(!middleWord.isEmpty) + apply(leftMinusItsLastWord) + apply(rightMinusItsFirstWord)
    }
  }
}
