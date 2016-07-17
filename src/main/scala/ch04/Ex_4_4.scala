package ch04

object Ex_4_4 {

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (tt => hh :: tt))
  }

}
