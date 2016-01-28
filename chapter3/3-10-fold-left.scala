
/**
    EXERCISE 10: foldRight is not tail-recursive and will
    StackOverflow for large lists. Convince yourself that
    this is the case, then write another general list-recursion
    function, foldLeft that is tail-recursive, using the
    techniques we discussed in the previous chapter.
*/

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /* solution to exercise */
  @annotation.tailrec
  def fold_left[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => fold_left(t, f(z, h))(f)
  }
}

/* Driver - Basic Test */
val a: List[Int] = List.apply(1)
val b: List[Int] = List.apply(1, 2)
println("Should be 'Cons(1,Nil)':         " + a)
println("Should be 'Cons(1,Cons(2,Nil))': " + b)

/* Driver - Exercise Test */
println("Should be '3':                   " + List.fold_left(b, 0)(_ + _))
println("Should be 'Cons(2,Cons(1,Nil))': " +
  "       " + List.fold_left(b, Nil: List[Int])((h, t) => Cons(t, h)) )

