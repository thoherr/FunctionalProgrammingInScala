package ch03

sealed trait MyTree[+A]

case class Leaf[A](value: A) extends MyTree[A]

case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

  // Exercise 3.25
  def size[A](t: MyTree[A]) : Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Exercise 3.26
  def maximum(t: MyTree[Int]) : Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27
  def depth[A](t: MyTree[A]) : Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28
  def map[A, B](t: MyTree[A])(f: A => B) : MyTree[B] = t match {
    case Leaf(l) => Leaf(f(l))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29
  def fold[A, B](t: MyTree[A])(f: A => B)(g: (B, B) => B) : B = t match {
    case Leaf(l) => f(l)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: MyTree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: MyTree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: MyTree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  def mapViaFold[A,B](t: MyTree[A])(f: A => B): MyTree[B] =
    fold(t)(a => Leaf(f(a)): MyTree[B])(Branch(_,_))

}
