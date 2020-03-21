package ar.com.flow.kata.pascaltriangle

/**
  * Pascal Triangle diagonal sum kata: https://www.codewars.com/kata/559b8e46fa060b2c6a0000bf
  *
  * This solution doesn't comply with performance requirements defined by Codewars.
  * However, I implemented the kata this way as an OOP exercise.
  */
import ar.com.flow.kata.pascaltriangle.PascalTriangle.topRow
import ar.com.flow.kata.pascaltriangle.PositionRelativeToEdges.{LeftEdge, RightEdge, Top}

object PascalTriangle {
  val topRow: Int = 0
}

object Diagonal {
  def diagonal(n: Int, p: Int): BigInt = {
    Diagonal(n, p).sum
  }
}

case class Diagonal(row: Int, diagonal: Int) {
  def nodes: List[Node] = {
    val start = Node(row, column = diagonal + 1)
    val upperNodes = start.rightParent.map(node => Diagonal(node.coordinates.row, diagonal)).map(_.nodes).getOrElse(Nil)
    start :: upperNodes
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
    PositionRelativeToEdges.of(coordinates) match {
      case Top() | LeftEdge() => None
      case RightEdge()        => Some(UpperRowRightMostColumnNode.from(coordinates))
      case _                  => Some(UpperRowLeftColumnNode.from(coordinates))
    }
  }
}

object RightParent {
  def of(coordinates: Coordinates): Option[Node] = {
    PositionRelativeToEdges.of(coordinates) match {
      case Top() | RightEdge() => None
      case _                   => Some(UpperRowSameColumnNode.from(coordinates))
    }
  }
}

object UpperRowSameColumnNode {
  def from(coordinates: Coordinates): Node = {
    Node(coordinates.upperRow, coordinates.column)
  }
}

object UpperRowRightMostColumnNode {
  def from(coordinates: Coordinates): Node = {
    Node(coordinates.upperRow, coordinates.row)
  }
}

object UpperRowLeftColumnNode {
  def from(coordinates: Coordinates): Node = {
    Node(coordinates.upperRow, coordinates.leftColumn)
  }
}

case class Coordinates(row: Int, column: Int) {
  val upperRow: Int = row - 1
  val leftColumn: Int = column - 1
}

sealed abstract class PositionRelativeToEdges extends Product with Serializable

object PositionRelativeToEdges {

  final case class Top() extends PositionRelativeToEdges

  final case class LeftEdge() extends PositionRelativeToEdges

  final case class RightEdge() extends PositionRelativeToEdges

  final case class Internal() extends PositionRelativeToEdges

  def of(coordinates: Coordinates): PositionRelativeToEdges = {
    coordinates match {
      case Coordinates(`topRow`, _)                      => Top()
      case Coordinates(_, 1)                             => LeftEdge()
      case Coordinates(row, column) if column == row + 1 => RightEdge()
      case _                                             => Internal()
    }
  }
}

