package ch04

object Ex_4_3 {

  def myMap2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (_, None) => None
    case (None, _) => None
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
  }

  // This is the official solution
  // I have to get used to this function combination stuff...
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

}
