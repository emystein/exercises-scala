package ar.com.flow.kata

// https://www.codewars.com/kata/559b8e46fa060b2c6a0000bf

import PascalTriangle.topRow

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

object Position {
  def of(node: Node): Position = Position(node.row, node.column)
}

case class Position(row: Int, column: Int) {
  val isInTheTopRow: Boolean = row == topRow
  val isInTheFirstColumn: Boolean = column == 1
  val isInTheLastColumn: Boolean = column == row + 1
}

object LeftParent {
  def of(node: Node): Option[Node] =
    if (node.position.isInTheTopRow || node.position.isInTheFirstColumn) {
      None
    } else if (node.position.isInTheLastColumn) {
      Some(Node(node.previousRow, column = node.row))
    } else {
      Some(Node(node.previousRow, node.previousColumn))
    }
}

object RightParent {
  def of(node: Node): Option[Node] =
    if (node.position.isInTheTopRow || node.position.isInTheLastColumn) {
      None
    } else {
      Some(Node(node.previousRow, node.column))
    }
}