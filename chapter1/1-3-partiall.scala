/**
    Ben Campbell
    August 17th 2015

    EXERCISE 3 (hard): Implement partial1 and write down a concrete usage of
    it. There is only one possible implementation that compiles. We don't have
    any concrete types here, so we can only stick things together using the
    local 'rules of the universe' established by the type signature. The style
    of reasoning required here is very common in functional programming—we are
    simply manipulating symbols in a very abstract way, similar to how we would
    reason when solving an algebraic equation.

    Partial Application -› function is being applied to some but not all args
      *    args: a value and a function of two arguments
      *    returns: function of one argument
*/

/** Actual Code */
 object Partial1 {

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    /** f(a,_)  */      // f takes A and returns itself with B unfilled
    (b: B) => f(a, b)   // better than original solution because b: B is exact
  }
  /* Driver: the partial func will fill arguments one-at-a-time */
  def main(args: Array[String]) = {
    /** Constants for Driver */
    val x: Int = 5
    val y: Int = 4

    val func = partial1(x: Int, (a: Int, b: Int) => (a - b): Int)

    if (func(y) == (x - y))
        println("Success!")
  }
}

