package ar.com.flow.kata.pascaltriangle

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

  def isEdge: Boolean = presentParents.size < 2

  def value: Int = presentParents.map(_.value).sum.max(1)
}
