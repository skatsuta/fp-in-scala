package functional_data_structures

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Prop.forAll

class TreeSpec extends Specification with ScalaCheck {
  //===== Exercise 3.25 =====
  "size" should {
    "return 1 when only one node exists" ! forAll { n: Int => Leaf(n).size must_== 1 }
    "return 2 when a branch has two leaves" ! forAll { (n: Int, m: Int) =>
      Branch(Leaf(n), Leaf(m)).size must_== 2
    }
    "return 3 when a branch has a leaf and a branch that has two leaves " !
      forAll { n: Int => Branch(Leaf(n), Branch(Leaf(n), Leaf(n))).size must_== 3 }
  }

  //===== Exercise 3.26 =====
  "maximum" should {
    "return the maximum element in a Tree[Int]" ! forAll { (a: Int, b: Int, c: Int) =>
      Branch(Branch(Leaf(a), Leaf(b)), Leaf(c)).maximum must_== (a max b max c)
    }

    "throw exception when a list is not Int" in {
      Leaf[String]("foo").maximum must throwA[UnsupportedOperationException]
    }
  }
}
