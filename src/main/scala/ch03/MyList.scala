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

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

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
  def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  // Exercise 3.6
  def init[A](l: MyList[A]): MyList[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => l
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: MyList[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ds: MyList[Double]) =
    foldRight(ds, 1.0)(_ * _)

  // Exercise 3.7
  // IMO the only way to stop immediately when encountering 0.0 in product2 would be to introduce another
  // parameter to foldRight that indicated the end of the recursion (give the null element into the fold)

  // Exercise 3.8
  val ex38 = foldRight(MyList(1, 2, 3), Nil: MyList[Int])(Cons(_, _))
  // foldRight exactly matches the structure of the list constructors

  // Exercise 3.9
  def length[A](as: MyList[A]): Int =
    foldRight(as, 0)((x, y) => 1 + y)

  // Exercide 3.10
  def foldLeftFirstAttempt[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(l: MyList[A], acc: B): B = l match {
      case Nil => acc
      case Cons(x, xs) => loop(xs, f(acc, x))
    }
    loop(as, z)
  }

  // Exercide 3.10
  // This is the sample solution. We apparently don't need an internal loop... ;-)
  @annotation.tailrec
  def foldLeft[A, B](l: MyList[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum3(ns: MyList[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ds: MyList[Double]) =
    foldLeft(ds, 1.0)(_ * _)

  def length3[A](as: MyList[A]): Int =
    foldLeft(as, 0)((x, y) => x + 1)

  // Exercise 3.12
  def reverse[A](as: MyList[A]): MyList[A] =
    foldLeft(as, Nil: MyList[A])((xs, x) => Cons(x, xs))

  // Exercise 3.13
  // foldRight in terms of foldLeft is actually simple if you get the idea...
  // reverse the list (tail recursive) and process it from left to right (tail recursive)
  def foldRightWithFoldLeft[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  // Exercise 3.14
  def append[A](as: MyList[A], bs: MyList[A]): MyList[A] =
    foldRight(as, bs)(Cons(_, _))

  // Exercise 3.15
  def concat[A](ll: MyList[MyList[A]]): MyList[A] =
    foldRight(ll, Nil: MyList[A])(append(_, _))

  // Exercise 3.16
  def add1(l: MyList[Int]): MyList[Int] =
    foldRight(l, Nil: MyList[Int])((h, t) => Cons(h + 1, t))

  // Exercise 3.17
  def double2string(l: MyList[Double]): MyList[String] =
    foldRight(l, Nil: MyList[String])((h, t) => Cons(h.toString, t))

  // Exercise 3.18
  def map[A, B](as: MyList[A], f: A => B): MyList[B] =
    foldRight(as, Nil: MyList[B])((h, t) => Cons(f(h), t))

  // Exercise 3.19
  def filter[A](as: MyList[A], f: A => Boolean): MyList[A] =
    foldRight(as, Nil: MyList[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // Exercise 3.20
  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] =
    foldRight(as, Nil: MyList[B])((h, t) => append(f(h), t))

  // Exercise 3.21
  def filter2[A](as: MyList[A], f: A => Boolean): MyList[A] =
    flatMap(as)(a => if (f(a)) MyList(a) else Nil)

  // Exercise 3.22
  def addPairs(as: MyList[Int], bs: MyList[Int]): MyList[Int] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (a, Nil) => a
    case (Nil, b) => b
    case (Cons(ah, at), Cons(bh, bt)) => Cons(ah + bh, addPairs(at, bt))
  }

  // Exercise 3.23
  def zipWith[A, B, C](as: MyList[A], bs: MyList[B])(f: (A, B) => C): MyList[C] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // Exercise 3.24
  @annotation.tailrec
  def startsWith[A](l: MyList[A], s: MyList[A]): Boolean = (l, s) match {
    case (_, Nil) => true
    case (Cons(hl, tl), Cons(hs, ts)) if hl == hs => startsWith(tl, ts)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }


}
