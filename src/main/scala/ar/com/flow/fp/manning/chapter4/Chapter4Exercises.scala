package ar.com.flow.fp.manning.chapter4

object Chapter4Exercises {
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val r = a.map(e => f(e) match {
      case None => return None
      case Some(e) => e
    })

    Option(r)
  }
}
