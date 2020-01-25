package ar.com.flow.fp.manning

object Chapter3Exercises {
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case h :: t => h :: append(t, a2)
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }
  }

  def lengthUsingFoldRight[A](as: List[A]): Long = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  def reverseUsingFoldRight[A](as: List[A]): List[A] = {
    foldRight[A, List[A]](as, Nil)((a, acc) => appendElement(acc, a))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def sumUsingFoldLeft(as: List[Long]): Long = {
    foldLeft[Long, Long](as, 0)((acc, a) => acc + a)
  }

  def productUsingFoldLeft(as: List[Long]): Long = {
    foldLeft[Long, Long](as, 0)((acc, a) => acc * a)
  }

  def lengthUsingFoldLeft(as: List[Long]): Long = {
    foldLeft[Long, Long](as, 0)((acc, _) => acc + 1)
  }

  def appendUsingFold[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((a, acc) => a :: acc)
  }

  def reverse[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: y :: Nil => List(y, x)
      case x :: y :: xs => y :: x :: reverse(xs)
    }
  }

  def appendElement[A](as: List[A], a: A): List[A] = {
    as match {
      case Nil => List(a)
      case x :: Nil => List(x, a)
      case x :: xs => x :: appendElement(xs, a)
    }
  }

  def last[A](as: List[A]): A = {
    as match {
      case x :: Nil => x
      case _ :: xs => last(xs)
    }
  }

  def add1(xs: List[Double]): List[Double] = {
    xs match {
      case Nil => Nil
      case x :: xs => (x + 1) :: add1(xs)
    }
  }

  def convertListOfDoubleToListOfString(xs: List[Double]): List[String] = {
    xs match {
      case Nil => Nil
      case x :: xs => x.toString :: convertListOfDoubleToListOfString(xs)
    }
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case x :: xs => f(x) :: map(xs)(f)
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case x :: xs => if (f(x)) x :: filter(xs)(f) else filter(xs)(f)
    }
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => Nil
      case x :: xs => foldRight(f(x), flatMap(xs)(f))((a, acc) => a :: acc)
    }
  }

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if (f(x)) List(x) else Nil)
  }

  def sumMerge(a1: List[Int], a2: List[Int]): List[Int] = {
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Nil, xs) => xs
      case (xs, Nil) => xs
      case (x :: xs, y :: ys) => (x + y) :: sumMerge(xs, ys)
    }
  }
  
  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = {
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Nil, xs) => xs
      case (xs, Nil) => xs
      case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
    }
  }
}
