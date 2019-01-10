package ar.com.flow.strings

import ar.com.flow.Monoids
import ar.com.flow.Monoids.{Part, Stub, WC}
import ar.com.flow.Strings._

object WordCountUsingWordCountMonoid {

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
      val (left, right) = string.splitAt(string.length / 2)
      val leftMinusWord = left.take(left.lastIndexOf(' ') + 1)
      val rightMinusWord = right.drop(right.indexOf(' '))
      val middleWordFound = (leftMinusWord + rightMinusWord) != string

      if (middleWordFound) {
        Monoids.wcMonoid.op(Monoids.wcMonoid.op(Part("", 1, ""), wordCount(leftMinusWord)), wordCount(rightMinusWord))
      } else {
        Monoids.wcMonoid.op(wordCount(left), wordCount(right))
      }
    }
  }
}
