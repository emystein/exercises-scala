package ar.com.flow.strings

import ar.com.flow.Monoids
import ar.com.flow.Monoids.{Part, Stub, WC}
import ar.com.flow.WCImplicits._

object WordCountUsingWCMonoidAndWCImplicits {

  def apply(string: String): Int = {
    reduceWordCount(string)
  }

  def reduceWordCount(wc: WC): WC = {
    wc match {
      case s@Stub(_) => s
      case Part(l, w, r) => Monoids.wcMonoid.op(Part("", w, ""), Monoids.wcMonoid.op(l, r))
    }
  }
}
