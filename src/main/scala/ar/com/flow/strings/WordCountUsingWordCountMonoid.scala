package ar.com.flow.strings

import ar.com.flow.Monoids
import ar.com.flow.Monoids.{Part, Stub, WC}

object WordCountUsingWordCountMonoid {
  def apply(string: String): Int = {
    if (string.isEmpty) {
      0
    } else if (string.contains(' ')) {
      val (l, r) = string.splitAt(string.length / 2)
//      val leftFound = (string.charAt(string.length / 2) != ' ') && !l.endsWith(" ")
//      val rightFound = (string.charAt(string.length / 2) != ' ') && !r.startsWith(" ")
//      val middleWordFound = (string.charAt(string.length / 2) != ' ') && leftFound && rightFound
//
//      val leftMinusWord = if (leftFound) l.take(l.lastIndexOf(' ')) else l
//      val rightMinusWord = if (rightFound) r.drop(r.indexOf(' ')) else r

      val middleWordFound = string.charAt(string.length / 2) != ' '
      val leftMinusWord = if (l.contains(' ') && !l.endsWith(" ")) l.take(l.lastIndexOf(' ') + 1) else l
      val rightMinusWord = if (r.contains(' ') && !r.startsWith(" ")) r.drop(r.indexOf(' ')) else r

      val wc: WC = Monoids.wcMonoid.op(recursiveWC(leftMinusWord), recursiveWC(rightMinusWord))

      wc match {
        case Stub(s) => apply(s)
        case Part(l, w, r) => {
          (if (middleWordFound) 1 else 0) + w
        }
      }
    } else {
      1
    }
  }

  val ltrim = (s: String) => s.replaceAll("^\\s+", "")
  val rtrim = (s: String) => s.replaceAll("\\s+$", "")

  def recursiveWC(input: String): WC = {
    val string = ltrim(rtrim(input))

    if (string.isEmpty) {
      Stub("")
    } else if (!string.contains(' ')) {
      Part("", 1, "")
    } else {
      val (l, r) = string.splitAt(string.length / 2)

      if (string.charAt(string.length / 2) != ' ') {
        val leftFound = !l.endsWith(" ")
        val rightFound = !r.startsWith(" ")
        val middleWordFound = leftFound && rightFound

        Monoids.wcMonoid.op(Monoids.wcMonoid.op(Part("", if (middleWordFound) 1 else 0, ""), recursiveWC(l)), recursiveWC(r))
      } else {
        Monoids.wcMonoid.op(recursiveWC(l), recursiveWC(r))
      }
    }
  }
}
