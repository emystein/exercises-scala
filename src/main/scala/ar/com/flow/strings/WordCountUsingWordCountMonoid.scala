package ar.com.flow.strings

import ar.com.flow.Monoids.{Part, Stub, WC}
import ar.com.flow.Strings._
import ar.com.flow.WCImplicits._
import ar.com.flow.strings.BoolImplicits._
import ar.com.flow.{Monoids, Strings}

object WordCountUsingWordCountMonoid {

  def apply(string: String): Int = {
    wordCount(string)
  }

  def wordCount(input: String): WC = {
    val string = ltrim(rtrim(input))

    if (string.isEmpty) {
      Stub("")
    } else if (!string.contains(' ')) {
      Part("", 1, "")
    } else {
      val (leftMinusItsLastWord, middleWord, rightMinusItsFirstWord) = Strings.splitByMiddleWord(string)

      val mergedWC = Monoids.wcMonoid.op(wordCount(leftMinusItsLastWord), wordCount(rightMinusItsFirstWord))

      Monoids.wcMonoid.op(Part("", !middleWord.isEmpty, ""), mergedWC)
    }
  }
}
