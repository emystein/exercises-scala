package ar.com.flow.kata

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
    } else if (row == topRow) {
      List(Node(row = topRow, column = 1))
    } else {
      val start = Node(row, column = diagonal + 1)
      val ns: List[Node] = start.rightParent.map(n => Diagonal(n.row, diagonal).nodes).getOrElse(Nil)
      start :: ns
    }

  def sum: Int = nodes.map(_.value).sum
}

case class Node(row: Int, column: Int) {
  def parents: (Option[Node], Option[Node]) = {
    if (row == topRow) {
      (None, None)
    } else if (column == 1) {
      (None, Some(Node(row = row - 1, column = 1)))
    } else if (column == row + 1) {
      (Some(Node(row = row - 1, column = row)), None)
    } else {
      (Some(Node(row = row - 1, column = column - 1)), Some(Node(row = row - 1, column = column)))
    }
  }

  def leftParent: Option[Node] = parents._1

  def rightParent: Option[Node] = parents._2

  def presentParents: Seq[Node] = Seq(parents._1, parents._2).flatten

  def value: Int = presentParents.map(_.value).sum.max(1)
}

