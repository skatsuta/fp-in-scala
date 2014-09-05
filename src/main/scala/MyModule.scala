// A comment!
/* Another comment */
/** A documentation comment */

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = n match {
      case n if n <= 0 => acc
      case _ => go(n - 1, n * acc)
    }
    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg format (n, factorial(n))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))

  def fib_normal(n: Int): Int =
    n match {
      case 0 | 1 => n
      case _ => fib_normal(n - 1) + fib_normal(n - 2)
    }

  def fib_tailrec(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, next: Int): Int = n match {
      case 0 => prev
      case 1 => next
      case _ => go(n - 1, next, prev + next)
    }
    go(n, 0, 1)
  }
}