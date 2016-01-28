
/**
    EXERCISE 12: Write a function that returns the reverse of a list
    (so given List(1,2,3) it returns List(3,2,1)). See if you can
    write it using a fold.
*/

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def fold_left[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => fold_left(t, f(z, h))(f)
  }

  /* Solution to Exercise */
  def reverse[A](l: List[A]): List[A] =
    fold_left(l, List[A]())((i, h) => Cons(h, i))

}

/* Driver for Exercise */
val a: List[Int] = List(1, 2, 3)
println("Should be 'Cons(3,Cons(2,Cons(1,Nil))): " + List.reverse(a))

// TODO: Understand better
