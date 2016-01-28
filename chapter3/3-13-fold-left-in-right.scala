
/**
    EXERCISE 13 (hard): Can you write foldLeft in terms of foldRight?
    How about the other way around?
*/

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty)
    else Cons(as.head, apply(as.tail: _*))
}

/* Driver for Exercise */
val a = List(1, 2, 3)
println(a)

