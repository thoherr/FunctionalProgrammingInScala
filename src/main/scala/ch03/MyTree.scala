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

}