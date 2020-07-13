package ar.com.flow.fp.manning.chapter6

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // A simple recursive solution
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def doubleUsingMap(s: Rand[Int])(f: Int => Double): Rand[Double] = {
    map(_.nextInt)(i => (i / (Int.MaxValue.toDouble + 1)))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def flatMap[A, B](ra: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = ra(rng)
    val rb = g(a)
    rb(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    def a(i: Int): Int = {
      val mod = i % n

      if (i + (n - 1) - mod >= 0) {
        mod
      } else {
        a(i)
      }
    }

    def ga(i: Int): Rand[Int] = unit(a(i))

    flatMap(nonNegativeInt)(ga)
  }

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    flatMap(s)(a => unit(f(a)))(rng)
  }
}
