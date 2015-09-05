
/**
    Ben Campbell
    September 5th 2015

    EXERCISE 9: Computer the length of a list using foldRight.
*/

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

class thisList extends List {
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, i) => i + 1)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

