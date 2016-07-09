package ch02

object Ex_2_1 {

  def fib(n: Int) = {
    @annotation.tailrec
    def go(n: Int, penultimate: Int, ultimate: Int): Int =
      if (n <= 0) penultimate
      else if (n == 1) ultimate
      else go(n - 1, ultimate, penultimate + ultimate)

    go(n, 0, 1)
  }
}
