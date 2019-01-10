package ar.com.flow

object Strings {
  def replaceSpaceWithUnderscore: String => String = { a: String => a.replaceAll(" ", "_") }
  def exclaimWords: String => String = { a: String => a.split(" ").filterNot(_.isEmpty).map(_ + "!").mkString(" ") }
  def toUpperCase: String => String = { a: String => a.toUpperCase }

  val concatenate: (String, String) => String = (s1: String, s2: String) => s1 + s2

  val ltrim = (s: String) => s.replaceAll("^\\s+", "")
  val rtrim = (s: String) => s.replaceAll("\\s+$", "")
}
