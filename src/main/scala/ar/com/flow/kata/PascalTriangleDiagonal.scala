package ar.com.flow.kata

// https://www.codewars.com/kata/559b8e46fa060b2c6a0000bf

import ar.com.flow.kata.PascalTriangle.topRow
import ar.com.flow.kata.PositionRelativeToEdges.{LeftEdge, RightEdge, Top}

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
      val ns: List[Node] = start.rightParent.map(n => Diagonal(n.coordinates.row, diagonal).nodes).getOrElse(Nil)
      start :: ns
    }

  def sum: Int = nodes.map(_.value).sum
}

object Node {
  def apply(row: Int, column: Int): Node = {
    Node(Coordinates(row, column))
  }
}

case class Node(coordinates: Coordinates) {
  def leftParent: Option[Node] = LeftParent.of(coordinates)

  def rightParent: Option[Node] = RightParent.of(coordinates)

  def parents: Seq[Node] = Seq(leftParent, rightParent).flatten

  def value: Int = parents.map(_.value).sum.max(1)
}

object LeftParent {
  def of(coordinates: Coordinates): Option[Node] = {
    PositionRelativeToEdges.of(CoordinatesPosition(coordinates)) match {
      case Top() | LeftEdge() => None
      case RightEdge()        => Some(Node(coordinates.previousRow, column = coordinates.row))
      case _                  => Some(Node(coordinates.previousRow, coordinates.previousColumn))
    }
  }
}

object RightParent {
  def of(coordinates: Coordinates): Option[Node] = {
    PositionRelativeToEdges.of(CoordinatesPosition(coordinates)) match {
      case Top() | RightEdge() => None
      case _                   => Some(Node(coordinates.previousRow, coordinates.column))
    }
  }
}
case class Coordinates(row: Int, column: Int) {
  val previousRow = row - 1
  val previousColumn = column - 1
}

case class CoordinatesPosition(coordinates: Coordinates) {
  val isTop: Boolean = coordinates.row == topRow
  val isLeftEdge: Boolean = coordinates.column == 1
  val isRightEdge: Boolean = coordinates.column == coordinates.row + 1
}

sealed abstract class PositionRelativeToEdges extends Product with Serializable

object PositionRelativeToEdges {

  final case class Top() extends PositionRelativeToEdges

  final case class LeftEdge() extends PositionRelativeToEdges

  final case class RightEdge() extends PositionRelativeToEdges

  final case class Inside() extends PositionRelativeToEdges

  def of(coordinatesPosition: CoordinatesPosition): PositionRelativeToEdges = {
    if (coordinatesPosition.isTop) {
      Top()
    } else if (coordinatesPosition.isLeftEdge) {
      LeftEdge()
    } else if (coordinatesPosition.isRightEdge) {
      RightEdge()
    } else {
      Inside()
    }
  }
}

