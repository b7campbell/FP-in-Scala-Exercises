
/**
    Ben Campbell
    August 17th 2015

    EXERCISE 1 (optional): Write a function to get the nth Fibonacci
    number. The first two Fibonacci numbers are 0 and 1, and the next
    number is always the sum of the previous two. Your definition should
    use a local tail-recursive function.
*/
val max = 12;

def fibonacci(n: Int): Int = {
  if (n == 0) return 0

  /* Next Fib: based on prev, return next */
  @annotation.tailrec
  def loop(iter: Int, fir: Int, sec: Int): Int = {
    if (iter == 0) fir
    else loop(iter - 1, sec, fir + sec)
  }
  loop(n, 0, 1)
}
def report_fibonacci(n: Int) =
  if (n == max) fibonacci(n) + "\n"
  else fibonacci(n) + ", "

/* Driver: Results should match standard Fibonacci sequence */
0 to max foreach { n => print(report_fibonacci(n)) }
println("0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144")  /* comparison to ensure correctness */

