package ar.com.flow.strings

import ar.com.flow.{Monoids, Strings}
import ar.com.flow.Monoids.{Part, Stub, WC}
import ar.com.flow.Strings._

object WordCountUsingWordCountMonoid {
  implicit def bool2int(b: Boolean) = if (b) 1 else 0

  def apply(string: String): Int = {
    val wc: WC = wordCount(string)

    wc match {
      case Stub(_) => 0
      case Part(_, w, _) => w
    }
  }

  def wordCount(input: String): WC = {
    val string = ltrim(rtrim(input))

    if (string.isEmpty) {
      Stub("")
    } else if (!string.contains(' ')) {
      Part("", 1, "")
    } else {
      val (leftMinusItsLastWord, middleWord, rightMinusItsFirstWord) = Strings.splitByMiddleWord(string)

      Monoids.wcMonoid.op(Monoids.wcMonoid.op(Part("", !middleWord.isEmpty, ""), wordCount(leftMinusItsLastWord)), wordCount(rightMinusItsFirstWord))
    }
  }

}
