package ar.com.flow

object BooleanImplicits {
  implicit def bool2int(b: Boolean) = if (b) 1 else 0
}
