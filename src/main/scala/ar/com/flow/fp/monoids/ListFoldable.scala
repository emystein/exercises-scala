package ar.com.flow.fp.monoids

class ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val fba = (b: B, a: A) => f(a, b)
    foldLeft(as)(z)(fba)
  }

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case x +: Nil => f(z, x)
    case x +: xs => f(foldLeft(xs)(z)(f), x)
  }

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
    foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
  }
}
