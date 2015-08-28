/**
    Ben Campbell
    August 17th 2015

    EXERCISE 5 (optional): Implement uncurry, which reverses the
    transformation of curry. Note that since => associates to the
    right, A => (B => C)can be written as A => B => C.

    Uncurrying

    convert a function that accepts one argument into one that
    accepts all bound variables at once
*/

object CurryingTest {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => (b => f(a, b))
  }
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def main(args: Array[String]) : Unit = {
    /* Constants for Driver */
    val x = 4
    val y = 5

    val f = uncurry((a: Int) => (b: Int) => (a + b): Int)
    val g = f(x, y)      /* function has one arg   */
    if (g == (x + y))
      println("Success!")
  }
}


