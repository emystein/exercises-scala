package ar.com.flow.fp.monoids

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

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as match {
      case Nil => m.zero
      case head +: tail => m.op(f(head), foldMap(tail, m)(f))
    }
  }
}
