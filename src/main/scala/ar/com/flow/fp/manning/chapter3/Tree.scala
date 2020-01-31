package ar.com.flow.fp.manning.chapter3

sealed trait Tree[+A] {
  def size: Int
}

case class Leaf[A](value: A) extends Tree[A] {
  def size: Int = 1
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def size: Int = 1 + left.size + right.size
}
