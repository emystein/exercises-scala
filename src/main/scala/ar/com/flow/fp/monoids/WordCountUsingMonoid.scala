package ar.com.flow.fp.monoids

import ar.com.flow.Strings
import ar.com.flow.Strings.{ltrim, rtrim}
import ar.com.flow.BooleanImplicits.bool2int
import ar.com.flow.fp.monoids.WordCount.wordCountMonoid

object WordCountUsingMonoid {

  def apply(string: String): Int = {
    wordCount(string) match {
      case Stub(_) => 0
      case Part(_, w, _) => w
    }
  }

  def wordCount(input: String): WordCount = {
    val string = ltrim(rtrim(input))

    if (string.isEmpty) {
      Stub("")
    } else if (!string.contains(' ')) {
      Part("", 1, "")
    } else {
      val (left, middleWord, right) = Strings.splitByMiddleWord(string)
      wordCountMonoid.op(Part("", !middleWord.isEmpty, ""), wordCountMonoid.op(wordCount(left), wordCount(right)))
    }
  }
}
