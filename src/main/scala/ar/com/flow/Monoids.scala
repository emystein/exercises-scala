package ar.com.flow

object Monoids {
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] = {
      if (a1.isDefined)
        a1
      else if (a2.isDefined)
        a2
      else
        zero
    }
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = { a: A => a }

    override def op(a1: A => A, a2: A => A): A => A = {
      a1.andThen(a2)
    }
  }

  def intSumMonoid: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0

    override def op(int1: Int, int2: Int): Int = int1 + int2
  }

  def stringConcatenateMonoid: Monoid[String] = new Monoid[String] {
    override def zero: String = ""

    override def op(s1: String, s2: String): String = s1 + s2
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as match {
      case Nil => m.zero
      case head +: tail => m.op(f(head), foldMap(tail, m)(f))
    }
  }

  // Balanced foldMap
  def foldMapV[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as match {
      case Nil => m.zero
      case head +: Nil => f(head)
      case head +: tail =>
        val (leftBranch, rightBranch) = as.splitAt(as.length / 2)
        m.op(foldMapV(leftBranch, m)(f), foldMapV(rightBranch, m)(f))
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def zero: WC = Stub("")

    override def op(wc1: WC, wc2: WC): WC = {
      (wc1, wc2) match {
        case (Stub(s1), Stub(s2)) => Part(s1, 0, s2)
        case (Part(left1, counter1, right1), Stub(s2)) => Part(left1, counter1, right1 + s2)
        case (Stub(s1), Part(left2, counter2, right2)) => Part(s1 + left2, counter2, right2)
        case (Part(left1, counter1, right1), Part(left2, counter2, right2)) => Part(left1 + left2, counter1 + counter2, right1 + right2)
      }
    }
  }
}
