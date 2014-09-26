package exceptions

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)

    // 別の実装例
    // map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this

    // 別の実装例
    // this map { Some(_) } getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None

    // 別の実装例
    // flatMap { x => if (f(x)) Some(x) else None }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](x: => A): Option[A] =
    try Some(x)
    catch { case e: Exception => None }

  //===== Exercise 4.3 =====
  def map2[B, C](b: Option[B])(f: (A, B) => C): Option[C] = this match {
    case None => None
    case Some(x) => b map { a => f(x, a) }
  }


}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  //===== Exercise 4.4 =====
//  def sequence[A](xs: List[Option[A]]): Option[List[A]] = xs.foldRight(Some(List[A]())) {
//    (x, acc) => x.lift2((a: A, as: List[A]) => a :: as)(acc)
//  }

//  def parseInts(ss: List[String]): Option[List[Int]] = ss.sequence(s map { i => Try(i.toInt) })

  //===== Exercise 4.5 =====
  //def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
}
