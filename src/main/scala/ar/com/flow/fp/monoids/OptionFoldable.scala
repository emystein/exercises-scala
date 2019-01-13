package ar.com.flow.fp.monoids

class OptionFoldable extends Foldable[Option] {
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None => z
      case Some(a) => f(z, a)
    }
}
