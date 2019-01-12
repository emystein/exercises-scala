package ar.com.flow.strings

object BoolImplicits {
  implicit def bool2int(b: Boolean) = if (b) 1 else 0
}
