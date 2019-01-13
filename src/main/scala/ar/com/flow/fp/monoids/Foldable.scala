package ar.com.flow.fp.monoids

trait Foldable[F[_]] {
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    val fba = (b: B, a: A) => f(a, b)
    foldLeft(as)(z)(fba)
  }

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
    foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
  }

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = {
    foldLeft(fa)(List[A]())((list, a) => a +: list)
  }
}
