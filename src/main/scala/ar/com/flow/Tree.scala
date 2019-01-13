package ar.com.flow

import ar.com.flow.fp.monoids.{Foldable, TraversableFoldable}

sealed trait Tree[+A] {
  def toList: List[A]
}

case class Leaf[A](value: A) extends Tree[A] {
  override def toList: List[A] = List(value)
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def toList: List[A] = right.toList ++ left.toList
}

class TreeFoldable extends Foldable[Tree] {
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(left, right) => {
      val treeAsList = right.toList ++ left.toList
      val listFoldable = new TraversableFoldable
      listFoldable.foldLeft(treeAsList)(z)(f)
    }
  }
}

