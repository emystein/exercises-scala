package ar.com.flow.fp.monoids

import ar.com.flow.BooleanImplicits._
import ar.com.flow.Strings.{ltrim, rtrim, splitByMiddleWord}

sealed trait WordCount

case class Stub(chars: String) extends WordCount
case class Part(lStub: String, words: Int, rStub: String) extends WordCount

object Part {
  def create(left: String, middleWord: String, right: String)= {
    Part(left, !middleWord.isEmpty, right)
  }
}

object WordCount {
  def wordCountMonoid = new Monoid[WordCount] {
    override def zero: WordCount = Stub("")

    override def op(wc1: WordCount, wc2: WordCount): WordCount = {
      (wc1, wc2) match {
        case (Stub(s1), Stub(s2)) => Part(s1, 0, s2)
        case (Part(left1, counter1, right1), Stub(s2)) => Part(left1, counter1, right1 + s2)
        case (Stub(s1), Part(left2, counter2, right2)) => Part(s1 + left2, counter2, right2)
        case (Part(left1, counter1, right1), Part(left2, counter2, right2)) => Part(left1 + left2, counter1 + counter2, right1 + right2)
      }
    }
  }
}

object WordCountImplicits {
  implicit def stringToWordCount(input: String): WordCount = {
    val string = ltrim(rtrim(input))

    if (string.isEmpty) {
      Stub("")
    } else if (!string.contains(' ')) {
      Part("", 1, "")
    } else {
      (Part.create _).tupled(splitByMiddleWord(string))
    }
  }
}