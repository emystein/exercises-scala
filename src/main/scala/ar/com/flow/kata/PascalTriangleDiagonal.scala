package ar.com.flow.kata

// https://www.codewars.com/kata/559b8e46fa060b2c6a0000bf

import ar.com.flow.kata.PascalTriangle.topRow
import ar.com.flow.kata.Position.{LeftEdge, RightEdge, Top}

object PascalTriangle {
  val topRow: Int = 0
}

object Diagonal {
  def diagonal(n: Int, p: Int): BigInt = {
    Diagonal(n, p).sum
  }
}

case class Diagonal(row: Int, diagonal: Int) {
  def nodes: List[Node] =
    if (row == topRow && diagonal != 0) {
      List.empty
    } else {
      val start = Node(row, column = diagonal + 1)
      val ns: List[Node] = start.rightParent.map(n => Diagonal(n.row, diagonal).nodes).getOrElse(Nil)
      start :: ns
    }

  def sum: Int = nodes.map(_.value).sum
}

case class Node(row: Int, column: Int) {
  val previousRow = row - 1
  val previousColumn = column - 1

  val position: Position = Position.of(this)

  def leftParent: Option[Node] = LeftParent.of(this)

  def rightParent: Option[Node] = RightParent.of(this)

  def parents: Seq[Node] = Seq(leftParent, rightParent).flatten

  def value: Int = parents.map(_.value).sum.max(1)
}

sealed abstract class Position extends Product with Serializable

object Position {
  final case class Top() extends Position
  final case class LeftEdge() extends Position
  final case class RightEdge() extends Position
  final case class Inside() extends Position

  def of(node: Node): Position = {
    val isTop: Boolean = node.row == topRow
    val isLeftEdge: Boolean = node.column == 1
    val isRightEdge: Boolean = node.column == node.row + 1

    if (isTop) {
      Top()
    } else if (isLeftEdge) {
      LeftEdge()
    } else if (isRightEdge) {
      RightEdge()
    } else {
      Inside()
    }
  }
}

object LeftParent {
  def of(node: Node): Option[Node] = {
    Position.of(node) match {
      case Top() | LeftEdge() => None
      case RightEdge()        => Some(Node(node.previousRow, column = node.row))
      case _                  => Some(Node(node.previousRow, node.previousColumn))
    }
  }
}

object RightParent {
  def of(node: Node): Option[Node] = {
    Position.of(node) match {
      case Top() | RightEdge() => None
      case _                   => Some(Node(node.previousRow, node.column))
    }
  }
}