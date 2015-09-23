
/**
    Simple demonstration of appending two lists together
    efficiently using data sharing via pattern matching.
*/

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](as1: List[A], as2: List[A]): List[A] = as1 match {
    case Nil => as2
    case Cons(h, t) => Cons(h, append(t, as2))
  }
}

/* Driver */
val a = List.apply(1, 2, 3)
val b = List.apply(4, 5, 6)

println("Should be 'Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))'\n"
        + "\t   " + List.append(a, b))

