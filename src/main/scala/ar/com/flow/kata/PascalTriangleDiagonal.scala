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
  private val previousRow = row - 1
  private val previousColumn = column - 1

  private val rowIsTop: Boolean = row == topRow
  private val columnIsTheLast: Boolean = column == row + 1

  def leftParent: Option[Node] =
    if (rowIsTop || column == 1) {
      None
    } else if (columnIsTheLast) {
      Some(Node(previousRow, column = row))
    } else {
      Some(Node(previousRow, previousColumn))
    }

  def rightParent: Option[Node] =
    if (rowIsTop || columnIsTheLast) {
      None
    } else {
      Some(Node(previousRow, column))
    }

  def parents: Seq[Node] = Seq(leftParent, rightParent).flatten

  def value: Int = parents.map(_.value).sum.max(1)
}
