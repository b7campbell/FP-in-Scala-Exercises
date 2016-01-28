
/**
    EXERCISE 9: Compute the length of a list using foldRight.
*/


/* Define a simple List type */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def fold_right[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, fold_right(t, z)(f))
  }

  /* Solution to Exercise */
  def length[A](l: List[A]): Int =
    fold_right(l, 0)((_, i) => i + 1)
}

/* Driver - Basic Test */
val a: List[Int] = List.apply(1)
val b: List[Int] = List.apply(1, 2)
println("Should be 'Cons(1, Nil)':        " + a)
println("Should be 'Cons(1,Cons(2,Nil))': " + b)

/* Driver - Exercise Test */
println("Should be '2':                   " + List.length(b))
