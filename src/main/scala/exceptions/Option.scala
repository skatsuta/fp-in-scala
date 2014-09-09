package exceptions

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

//  def flatMap[B](f: A => Option[B]): Option[B]
//  def getOrElse[B >: A](default: => B): Option[B]
//  def orElse[B >: A](ob: => B): Option[B]
//  def filter(f: A => Boolean): Option[A]
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Util {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}

