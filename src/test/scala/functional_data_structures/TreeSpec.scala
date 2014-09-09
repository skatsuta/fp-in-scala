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
  }
}
