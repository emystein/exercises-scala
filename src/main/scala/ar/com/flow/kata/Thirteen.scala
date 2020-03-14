package ar.com.flow.kata

// https://www.codewars.com/kata/564057bc348c7200bd0000ff
object Thirteen {
  val pattern = Array(1, 10, 9, 12, 3, 4)

  def thirt(n: Long): Long = {
    n.toString.reverse.zipWithIndex
      .map { case (c, i) => pattern(i % 6) * c.asDigit }
      .sum match {
      case `n` => n
      case x => thirt(x)
    }
  }
}
