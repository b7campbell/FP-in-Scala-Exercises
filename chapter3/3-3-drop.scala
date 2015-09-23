/**
    EXERCISE 3: Generalize tail to the function drop,
    which removes the first n elements from a list.
*/

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  /* solution to exercise */
  def drop[A](n: Int, as: List[A]): List[A] =
    if ( n <= 0 ) as
    else as match {
      case Nil => Nil
      case Cons(_, t) => drop(n - 1, t)
    }
}
