package ar.com.flow.fp.monoids

class ListFoldable extends Foldable[List] {
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case x +: Nil => f(z, x)
    case x +: xs => f(foldLeft(xs)(z)(f), x)
  }
}
