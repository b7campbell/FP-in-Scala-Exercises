
/**
    Ben Campbell
    August 27th 2015

    EXERCISE 3: Generalize tail to the function drop, which
    removes the first n elements from a list.
*/

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, as) => as  /* convention to use _ for unused data */
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    if (n <= 0) return as
    else as match {
      case Nil => sys.error("Asked to drop too many items")
      case Cons(_, t) => drop(t, n - 1)
    }
  }
}



