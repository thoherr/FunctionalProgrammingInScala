package ch02

object Ex_2_1 {

  def fib(n: Int) = {
    @annotation.tailrec
    def go(n: Int, penultimate : Int, ultimate: Int) : Int =
      if (n <= 0) penultimate
      else if (n == 1) ultimate
      else go(n-1, ultimate, penultimate+ultimate)

    go(n, 0, 1)
  }
}

object Ex_2_2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
    @annotation.tailrec
    def loop(n: Int) : Boolean =
      if (n >= as.length) true
      else if (!ordered(as(n-1), as(n))) false
      else loop(n+1)
    loop(1)
  }
}

object Ex_2_3 {
  def curry[A,B,C](f: (A,B) => C) : A => (B => C) =
    a => b => f(a, b)
}

object Ex_2_4 {
  def uncurry[A,B,C](f: A => B => C) : (A, B) => C =
    (a, b) => f(a)(b)
}

object Ex_2_5 {
  def compose[A,B,C](f: A => B, g: B => C) : A => C =
    a => g(f(a))
}
