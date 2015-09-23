
/**
      EXERCISE 4: Implement dropWhile, which removes elements
      from the List prefix as long as they match a predicate.
      Again, notice these functions take time proportional
      only to the number of elements being dropped—we do not
      need to make a copy of the entire List.
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

    /* solution to exercise */
    def drop_while[A](as: List[A], f: A => Boolean): List[A] = as match {
      case Cons(h, t) if f(h) => drop_while(t, f)
      case _ => as
    }
}

/* Driver */
val a: List[Int] = List.apply(1, 2, 3, 4, 5)
println("Should be 'Cons(4, Cons(5, Nil))': " + List.drop_while(a, (x: Int) => x < 4 ))
println("Should be 'Cons(5, Nil)':          " + List.drop_while(a, (x: Int) => x != 5 ))

