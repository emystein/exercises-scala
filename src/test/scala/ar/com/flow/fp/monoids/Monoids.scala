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
}
