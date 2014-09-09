package functional_data_structures

sealed trait Tree[+A] {  // 型パラメータの前に共変アノテーション(+)をつけると共変になる
  //===== Exercise 3.25 =====
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => l.size + r.size + 1
  }

  //===== Exercise 3.26 =====
  def maximum: Int = this match {
    case Leaf(x: Int) => x
    case Branch(l, r) => l.maximum max r.maximum
    case _ => throw new UnsupportedOperationException("type parameter is invalid.")
  }

  //===== Exercise 3.27 =====
  def depth: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => (l.depth max r.depth) + 1
    // max の呼び出しより + の呼び出しが優先されるため、max 部分を括弧で囲む必要がある
    // 呼び出しの優先順位はメソッド名の1文字目によって判定され、以下の通り
    // 1. 以下以外のすべての特殊文字 (~ # など)
    // 2. * %
    // 3. + -
    // 4. :
    // 5. = !
    // 6. < >
    // 7. &
    // 8. ^
    // 9. |
    // 10. すべての英字
    // 11. すべて代入演算子
  }

  //===== Exercise 3.28 =====
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(l map f, r map f)
  }

  //===== Exercise 3.29 =====
  def fold[B](f: A => B)(g: (B, B) => B): B = this match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(l.fold(f)(g), r.fold(f)(g))
  }
  def sizeViaFold: Int = fold(_ => 1)(_ + _ + 1)
  //def maximumViaFold(implicit evidence: A =:= Int): Int =  fold(x => x)(_ max _)
  //def depthViaFold: Int = fold(_ => 1)((_ max _) + 1)
  def mapViaFold[B](f: A => B): Tree[B] = fold(x => Leaf(f(x)): Tree[B])(Branch(_, _))
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
