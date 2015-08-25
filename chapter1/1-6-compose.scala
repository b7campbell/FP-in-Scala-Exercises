/**
    Ben Campbell
    August 17th 2015

    EXERCISE 6: Implement the higher-order function that composes
    two functions.

    Composing Two Functions

    Returns a new function that, when called, will apply g to the result
    of f.
*/

object compose {

    def compose[A,B,C](f: B => C, g: A => B): A => C = {
      (a: A) => f(g(a))
    }
    def main(args: Array[String]) = {
      val f: (Int => Int) = (a: Int) => (a + a)
      val g: (Int => Int) = (b: Int) => b

      val a: (Int => Int) = compose(g, f)
      if (a(10) == f(g(10)))
        println("Success!")
    }
}


