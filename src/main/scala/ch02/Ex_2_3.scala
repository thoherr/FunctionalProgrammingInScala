package ch02

object Ex_2_3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)
}

