
/**
    EXERCISE 2: Implement the function tail for "removing" the first
    element of a List. Notice the function takes constant time. What
    are different choices you could make in your implementation if the
    List is Nil? We will return to this question in the next chapter.
*/

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /* solution to the exercise */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }
}


