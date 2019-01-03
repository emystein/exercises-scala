package ar.com.flow.fp.monoids

object Strings {
  def replaceSpaceWithUnderscore: String => String = { a: String => a.replaceAll(" ", "_") }
  def exclaimWords: String => String = { a: String => a.split(" ").filterNot(_.isEmpty).map(_ + "!").mkString(" ") }
  def toUpperCase: String => String = { a: String => a.toUpperCase }
}
