package ar.com.flow

object StringAnalysis {
  def wordInMiddle(string: String): Boolean = !string.isEmpty && string.charAt(string.length / 2) != ' '
}
