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

    // Another implementation
    //------------------------
    // if (xs1.isEmpty) xs2
    // else xs1.head :: append(xs.tail, xs2)

    // List#:::[B >: A](prefix: List[A]): List[B] =
    //   if (isEmpty) prefix
    //   else if (prefix.isEmpty) this
    //   else (new ListBuffer[B] ++= prefix).prependToList(this)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case Nil | List(_) => Nil
    case _ => xs.head :: init(xs.tail)
  }

  def dropWhile2[T](xs: List[T])(f: T => Boolean): List[T] = xs match {
    case x :: xs1 if f(x) => dropWhile2(xs1)(f)
    case _ => xs
  }

  def foldRight[T, U](xs: List[T])(acc: U)(f: (T, U) => U): U = xs match {
    case Nil => acc
    case x :: xs1 => f(x, foldRight(xs1)(acc)(f))

    // TraversableOnce#foldRight[B](acc: B)(f: (A, B) => B): B =
    //   reversed.foldLeft(acc)((x, y) => f(y, x))
  }

  def sum2(xs: List[Int]): Int = foldRight(xs)(0)(_ + _)

  def product2(xs: List[Double]): Double = foldRight(xs)(1.0)(_ * _)

  // *** Exercise 3.7 ***
  // Can short-circuit because we can check the condition before calculating
  def product3(xs: List[Double]): Double = xs match {
    case Nil => 1.0
    case x :: _ if x == 0.0 => 0.0
    case x :: xs1 => x * product3(xs1)

    // GenTraversableOnce#product(List[])
    //------------------------------------
    // def product[B >: A](implicit num: Numeric[B]) = foldLeft(num.one)(num.times)
  }


  //===== Exercise 3.9 =====
  def length[T](xs: List[T]): Int = foldRight(xs)(0)((_, acc) => acc + 1)

    // LinearSeqOptimized#length(List[T])
    //------------------------------------
    // var these = self
    // var len = 0
    // while(!these.isEmpty) {
    //   len += 1
    //   these = these.tail
    // }
    // len


  //===== Exercise 3.10 =====
  @annotation.tailrec
  def foldLeft[A, B](xs: List[A])(acc: B)(f: (B, A) => B): B = xs match {
    case Nil      => acc
    case x :: xs1 => foldLeft(xs1)(f(acc, x))(f)

    // TraversableOnce#foldLeft[A, B](acc: B)(f: (B, A) => B): B = {
    //   var result = acc
    //   this foreach (x => result = f(result, x))
    //   result
    // }
  }

  //===== Exercise 3.11 =====
  def sum(xs: List[Int]): Int = foldLeft(xs)(0)(_ + _)
  def product(xs: List[Double]): Double = foldLeft(xs)(1.0)(_ * _)
  def length2[A](xs: List[A]): Int = foldLeft(xs)(0)((acc, _) => acc + 1)


  //===== Exercise 3.12 =====
  def reverse[A](xs: List[A]): List[A] = foldLeft(xs)(List[A]())((acc, x) => x :: acc)
}
