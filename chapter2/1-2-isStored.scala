/**
    Ben Campbell
    August 17th 2015

    EXERCISE 2: Implement isSorted, which checks whether an Array[A]
    is sorted according to a given comparison function.
*/

object Sort {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n+1))) false
      else go(n + 1)
    }
    go(0)
  }
  def gti(a: Int, b: Int): Boolean = {
    if (a > b) true
    else false
  }
  def gtb(a: Boolean, b: Boolean): Boolean = {
    if (a == false && b == false) false
    else if (a == false && b == true) false
    else if (a == true && b == true) false
    else true
  }
  def gtd(a: Double, b: Double): Boolean = {
    if (a > b) true
    else false
  }
  def main(args: Array[String]): Unit = {
    val a: Array[Int] = Array(1, 2, 3)
    val b: Array[Double] = Array(4.0, 5.0, 3.0)
    val c: Array[Boolean] = Array(false, false, true)
    println(isSorted(a, gti))
    println(isSorted(b, gtd))
    println(isSorted(c, gtb))
  }
}


