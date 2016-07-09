package ch03

sealed trait MyList[+A]

case object Nil extends MyList[Nothing]

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // Exercise 3.2
  def tail[A](ls: MyList[A]): MyList[A] = ls match {
    case Nil => Nil // could be an error
    case Cons(x, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](ls: MyList[A], h: A): MyList[A] = ls match {
    case Nil => Cons(h, ls) // Could also be an error
    case Cons(x, xs) => Cons(h, xs)
  }

  // Execrcise 3.4
  @annotation.tailrec
  def drop[A](l: MyList[A], n: Int): MyList[A] =
    if (n <= 0) l
    else drop(tail(l), n - 1)

  // Exercise 3.5
  def dropWhile[A](l:MyList[A], f: A => Boolean) : MyList[A] = l match {
    case Nil => Nil  // This could also be an error
    case Cons(x, xs) => if (f(x)) dropWhile(tail(l), f) else l
  }

  // Exercise 3.6
  def init[A](l: MyList[A]) : MyList[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
