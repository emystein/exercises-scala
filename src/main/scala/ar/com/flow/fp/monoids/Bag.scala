package ar.com.flow.fp.monoids

object Bag {
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
