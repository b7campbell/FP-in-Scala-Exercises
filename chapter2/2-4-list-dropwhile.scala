
/**
    Ben Campbell
    August 27th 2015

    EXERCISE 4: Implement dropWhile, which removes elements from
    the List prefix as long as they match a predicate. Again, notice
    these functions take time proportional only to the number of
    elements being dropped—we do not need to make a copy of the entire
    List.
*/

/* Sidebar Notes: Type Inference in Scala

    first argument group -> List itself
    second argument group -> any functions to be received

    the two groups necessitate two parentheses when calling

    using this syntax, Scala can determine type without annotation
    (makes passing function literals much easier)
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
}

