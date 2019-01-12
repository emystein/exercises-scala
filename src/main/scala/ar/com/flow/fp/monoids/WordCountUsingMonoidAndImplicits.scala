package ar.com.flow.fp.monoids

import ar.com.flow.fp.monoids.WordCount.wordCountMonoid
import ar.com.flow.fp.monoids.WordCountImplicits.stringToWC

object WordCountUsingMonoidAndImplicits {

  def apply(string: String): Int = {
    reduce(string) match {
      case Stub(_) => 0
      case Part(_, w, _) => w
    }
  }

  def reduce(wc: WordCount): WordCount = {
    wc match {
      case s@Stub(_) => s
      case Part(l, w, r) => wordCountMonoid.op(Part("", w, ""), wordCountMonoid.op(l, r))
    }
  }
}
