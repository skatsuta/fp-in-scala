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
  // map2 ではなく lift2
  def lift2[B, C](f: A => B => C): Option[B] => Option[C] = this match {
    case None => _ => None
    case Some(x) => _ map f(x)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
