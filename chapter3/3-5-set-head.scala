
/**
      EXERCISE 5: Using the same idea, implement the
      function setHead for replacing the first element
      of a List with a different value.
*/

/* Define a simple List type */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/* companion object has methods for class above */
object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def set_head[A](n: A, as: List[A]): List[A] = as match {
      case Nil => Cons(n, Nil)
      case Cons(_, t) => Cons(n, t)
    }
}

/* Driver */
val a: List[Int] = List.apply()
val b: List[Int] = List.apply(1, 2)
println("Should be 'Cons(100, Nil)':        " + List.set_head(100, a))
println("Should be 'Cons(50, Cons(2, Nil)': " + List.set_head(50, b))
