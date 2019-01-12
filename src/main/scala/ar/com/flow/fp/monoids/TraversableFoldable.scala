package ar.com.flow.fp.monoids

class TraversableFoldable extends Foldable[Traversable] {
  override def foldLeft[A, B](as: Traversable[A])(z: B)(f: (B, A) => B): B = {
    if (as.isEmpty) {
      z
    } else {
      f(foldLeft(as.tail)(z)(f), as.head)
    }
  }
}
