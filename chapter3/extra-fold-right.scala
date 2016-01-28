
/**
    fold right demonstration with simple arithmetic
*/

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def fold_right[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, fold_right(t, z)(f))
  }

  def sum(l: List[Int]): Int =
    fold_right(l, 0)(_ + _)

  def product(l: List[Double]): Double =
    fold_right(l, 1.0)(_ * _)
}

/* Driver */
val a: List[Int] = List.apply(1, 2, 3)
println("Should be 'Cons(1,Cons(2,Cons(3,Nil)))': " + a)
println("Should be '6':   " + List.sum(a))

val a2: List[Double] = List.apply(1, 2, 3)
println("Should be '6.0': " + List.product(a2))

val b: List[Int] = List.apply()
println("Should be '0':   " + List.sum(b))
val b2: List[Double] = List.apply()
println("Should be '1.0': " + List.product(b2))
