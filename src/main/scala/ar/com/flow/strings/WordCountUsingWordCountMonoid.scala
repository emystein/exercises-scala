package ar.com.flow.strings

import ar.com.flow.Monoids
import ar.com.flow.Monoids.{Part, Stub, WC}
import ar.com.flow.Strings._

object WordCountUsingWordCountMonoid {

  def apply(string: String): Int = {
    val wc: WC = recursiveWC(string)

    wc match {
      case Stub(_) => 0
      case Part(_, w, _) => w
    }
  }

  def recursiveWC(input: String): WC = {
    val string = ltrim(rtrim(input))

    if (string.isEmpty) {
      Stub("")
    } else if (!string.contains(' ')) {
      Part("", 1, "")
    } else {
      val (left, right) = string.splitAt(string.length / 2)
      val leftMinusWord = if (left.contains(' ') && !left.endsWith(" ")) left.take(left.lastIndexOf(' ') + 1) else left
      val rightMinusWord = if (right.contains(' ') && !right.startsWith(" ")) right.drop(right.indexOf(' ')) else right
      val middleWordFound = (leftMinusWord + rightMinusWord) != string

      if (middleWordFound) {
        Monoids.wcMonoid.op(Monoids.wcMonoid.op(Part("", 1, ""), recursiveWC(leftMinusWord)), recursiveWC(rightMinusWord))
      } else {
        Monoids.wcMonoid.op(recursiveWC(left), recursiveWC(right))
      }
    }
  }
}
