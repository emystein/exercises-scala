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

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (A.zero, B.zero)

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K, V]()

    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
          b.getOrElse(k, V.zero)))
      }
  }

  /**
    * A bag is like a set, except that it’s represented by a map that contains one entry per element with that element as the key, and the value under that key is the number of times the element appears in the bag.<br/>
    * For example:<br/>
    * scala> bag(Vector("a", "rose", "is", "a", "rose"))<br/>
    * res0: Map[String,Int] = Map(a -> 2, rose -> 2, is -> 1)
    *
    * @param as the sequence
    * @tparam A the type of the elements in the sequence
    * @return a Map with A and the number of its occurrences in the sequence
    */
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val foldable = new TraversableFoldable

    foldable.foldMap(as)(a => Map(a -> 1))(Monoids.mapMergeMonoid[A, Int](Monoids.intSumMonoid))
  }
}
