package ar.com.flow.kata

import scala.util.Try

// https://www.codewars.com/kata/59590976838112bfea0000fa
object EnglishBeggars {
   def beggars(values: List[Int], n: Int): List[Int] = {
     Try(values.grouped(n).toList.map(_.padTo(n, 0)).transpose.map(_.sum)).getOrElse(Nil)
  }
}
