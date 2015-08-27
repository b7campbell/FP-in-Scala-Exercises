
/**
    Ben Campbell
    August 28th 2015

    EXERCISE 6: Not everything works out so nicely. Implement a
    function, init, which returns a List consisting of all but
    the last element of a List. So, given List(1,2,3,4), init will
    return List(1,2,3). Why can't this function be implemented in
    constant time like tail?

    This must walk the entire list to the end. O(n) not O(1)
*/

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, as) => as  /* convention to use _ for unused data */
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    if (n <= 0) return as
    else as match {
      case Nil => sys.error("Asked to drop too many items")
      case Cons(_, t) => drop(t, n - 1)
    }
  }
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case Nil => Nil
    case _ => l
  }
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Cons(_, t) => Cons(a, t)
    case Nil => sys.error("cannot setHead on empty list")
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => sys.error("init on empty list not possible")
  }
}



