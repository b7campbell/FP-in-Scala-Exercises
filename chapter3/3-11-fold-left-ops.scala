
/**
      EXERCISE 11: Write sum, product, and a function to compute
      the length of a list using foldLeft.
*/

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  @annotation.tailrec
  def fold_left[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => fold_left(t, f(z, h))(f)
  }

  /* solution to exercise */
  def sum(l: List[Int]): Int =
    fold_left(l, 0)((a: Int, b: Int) => a + b)

  def product(l: List[Double]): Double =
    fold_left(l, 1.0)((a: Double, b: Double) => a * b)

  def length[A](l: List[A]): Int =
    fold_left(l, 0)((i: Int, _ ) => i + 1)
}

/* Driver for Exercise */
val a: List[Int] = List(1, 2, 3)
println("Should be '6':   " + List.sum(a))

val b: List[Double] = List(1, 2, 3)
println("Should be '6.0': " + List.product(b))

println("Should be '3':   " + List.length(a))



