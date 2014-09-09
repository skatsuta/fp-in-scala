package functional_data_structures

sealed trait Tree[+A] {  // 型パラメータの前に共変アノテーション(+)をつけると共変になる
  //===== Exercise 3.25 =====
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(left, right) => left.size + right.size
  }
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
