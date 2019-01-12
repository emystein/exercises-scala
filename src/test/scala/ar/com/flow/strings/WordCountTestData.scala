package ar.com.flow.strings

import org.scalatest.prop.TableDrivenPropertyChecks

object WordCountTestData extends TableDrivenPropertyChecks {
  val table = Table(
    ("String", "Expected"),
    ("", 0),
    (" ", 0),
    ("  ", 0),
    ("   ", 0),
    ("1", 1),
    ("Hello", 1),
    ("Hello    ", 1),
    ("    Hello", 1),
    ("Hello     ", 1),
    ("     Hello", 1),
    ("Hello      ", 1),
    ("      Hello", 1),
    ("1 2", 2),
    ("12 3", 2),
    ("1 23", 2),
    ("Hello World", 2),
    ("Hello World ", 2),
    (" Hello World", 2),
    ("1 2 3", 3),
    ("1 23 4", 3),
    ("1 234 5 ", 3),
  )
}
