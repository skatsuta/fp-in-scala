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

    // FP in Scala の解答
    // ------------------
    // if (n <= 0) xs
    // else xs match {
    //   case Nil => Nil
    //   case _ :: xs => drop(xs, n - 1)
    // }

    // 実際の実装 (List#drop(Int) の実装)
    // ---------
    // var these = this
    // var count = n
    // while (!these.isEmpty && count > 0) {
    //   these = these.tail
    //   count -= 1
    // }
    // these
  }
}
