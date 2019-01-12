package ar.com.flow.strings

import ar.com.flow.Strings
import ar.com.flow.Strings._
import BoolImplicits._
import ar.com.flow.WCImplicits.reduceWCToNumber
import WordCountUsingWordCountMonoid.wordCount

object RecursiveWordCount {
  // TODO add tests
  def countWords(input: String): Int = {
    val string = ltrim(rtrim(input))

    if (string.isEmpty) {
      0
    } else if (!string.contains(' ')) {
      1
    } else {
      val (leftMinusItsLastWord, middleWord, rightMinusItsFirstWord) = Strings.splitByMiddleWord(string)

      bool2int(!middleWord.isEmpty) + wordCount(leftMinusItsLastWord) + wordCount(rightMinusItsFirstWord)
    }
  }
}
