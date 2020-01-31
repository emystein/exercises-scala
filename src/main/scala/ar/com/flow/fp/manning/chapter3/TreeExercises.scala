package ar.com.flow.fp.manning.chapter3

object TreeExercises {
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }
}
