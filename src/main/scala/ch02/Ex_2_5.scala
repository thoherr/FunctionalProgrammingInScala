package ch02

object Ex_2_5 {
  def compose[A,B,C](f: A => B, g: B => C) : A => C =
    a => g(f(a))
}
