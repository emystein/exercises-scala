package ar.com.flow.strings

object StringAnalysis {
  def wordInMiddle(string: String): Boolean = !string.isEmpty && string.charAt(string.length / 2) != ' '
}
