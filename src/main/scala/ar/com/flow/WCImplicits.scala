package ar.com.flow

import ar.com.flow.Monoids.{Part, Stub, WC}
import ar.com.flow.Strings._
import ar.com.flow.strings.BoolImplicits._

object WCImplicits {
  // TODO add tests
  implicit def stringToWC(input: String): WC = {
    val string = ltrim(rtrim(input))

    if (string.isEmpty) {
      Stub("")
    } else if (!string.contains(' ')) {
      Part("", 1, "")
    } else {
      val (leftMinusItsLastWord, middleWord, rightMinusItsFirstWord) = Strings.splitByMiddleWord(string)
      Part(leftMinusItsLastWord, !middleWord.isEmpty, rightMinusItsFirstWord)
    }
  }

  // TODO add tests
  implicit def reduceWCToNumber(wc: WC): Int = {
    wc match {
      case Stub(_) => 0
      case Part(l, w, r) => w + extractWordCountNumber(l) + extractWordCountNumber(r)
    }
  }

  // TODO add tests
  private def extractWordCountNumber(wc: WC): Int =
    wc match {
      case Stub(_) => 0
      case Part(_, w, _) => w
    }
}
