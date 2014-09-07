package functional_data_structures

object MyList extends {
  def tail[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case _ :: t => t
  }

  def setHead[T](xs: List[T], x: T): List[T] = xs match {
    case Nil => Nil
    case _ :: t => x :: t
  }

  @annotation.tailrec
  def drop[T](xs: List[T], n: Int): List[T] = (xs, n) match {
    case (l, m) if m <= 0 || l == Nil => l
    case (_ :: t, m) => drop(t, m - 1)

    // FP in Scala
    // ------------
    // if (n <= 0) xs
    // else xs match {
    //   case Nil => Nil
    //   case _ :: xs => drop(xs, n - 1)
    // }

    // List#drop(Int)
    // ---------------
    // var these = this
    // var count = n
    // while (!these.isEmpty && count > 0) {
    //   these = these.tail
    //   count -= 1
    // }
    // these
  }

  @annotation.tailrec
  def dropWhile[T](xs: List[T], f: T => Boolean): List[T] = xs match {
    case Nil => Nil
    case x :: xs1 => if (!f(x)) xs else dropWhile(xs1, f)

    // FP in Scala
    // ------------
    // case Cons(x, xs1) if f(x) => dropWhile(xs1, f)
    // case _ => xs

    // List#dropWhile(A => Boolean)
    // -----------------------------
    // @tailrec
    // def loop(xs: List[A]): List[A] = {
    //   if (xs.isEmpty || !p(xs.head)) xs
    //   else dropWhile(xs.tail)
    // }
    // loop(this)
  }

  def append[T](xs1: List[T], xs2: List[T]): List[T] = xs1 match {
    case Nil => xs2
    case x :: xs => x :: append(xs, xs2)

    // another implementation
    // if (xs1.isEmpty) xs2
    // else xs1.head :: append(xs.tail, xs2)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case Nil | (_ :: Nil) => Nil
    case _ => xs.head :: init(xs.tail)
  }
}
