package ar.com.flow.fp.manning.chapter3

object TreeExercises {
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  def depth(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }

  def map[A](tree: Tree[A])(f: A => A): Tree[A] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }
}
