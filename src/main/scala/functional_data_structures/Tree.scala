package functional_data_structures

sealed trait Tree[+A] {  // 型パラメータの前に共変アノテーション(+)をつけると共変になる
  //===== Exercise 3.25 =====
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => l.size + r.size
  }

  //===== Exercise 3.26 =====
  def maximum: Int = this match {
    case Leaf(x: Int) => x
    case Branch(l: Tree[Int], r: Tree[Int]) => l.maximum max r.maximum
    case _ => throw new UnsupportedOperationException("type parameter is invalid.")
  }
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
