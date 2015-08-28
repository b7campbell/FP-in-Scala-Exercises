/**
    Ben Campbell
    August 17th 2015

    EXERCISE 4 (hard): Let's look at another example, currying, which converts
    a function of N arguments into a function of one argument that returns
    another function as its result.11 Here again, there is only one
    implementation that typechecks.

    Currying

    convert a function of N arguments into a function of one arg
    that returns another function (it may be partial) as its result
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

    val f = curry((a: Int, b: Int) => (a + b): Int)
    val g = f(x)      /* function has one arg   */
    val h = g(y)      /* function has both args */
    val z = f(x)(y)   /* a one line alternative */
    if (h == (x + y))
      println("Success!")
  }
}


