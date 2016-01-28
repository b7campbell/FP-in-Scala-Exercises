
/**
      EXERCISE 6: Not everything works out so nicely. Implement
      a function, init, which returns a List consisting of all
      but the last element of a List. So, given List(1,2,3,4),
      init will return List(1,2,3). Why can't this function be
      implemented in constant time like tail?
*/

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /* solution to exercise */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Empty list cannot init")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}

/* Driver */
val a = List(1, 2, 3)
println("Should be 'Cons(1, Cons(2, Nil))': " + List.init(a))

val b = List()
println(List.init(b)) /* should throw error "Empty list cannot init" */
