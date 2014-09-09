package functional_data_structures

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Prop.forAll

class TreeSpec extends Specification with ScalaCheck {
  //===== Exercise 3.25 =====
  "size" should {
    "return 1 when only one node exists" ! forAll { n: Int => Leaf(n).size must_== 1 }
    "return 3 when a branch has two leaves" ! forAll { (n: Int, m: Int) =>
      Branch(Leaf(n), Leaf(m)).size must_== 3
    }
    "return 5 when a branch has a leaf and a branch that has two leaves " !
      forAll { n: Int => Branch(Leaf(n), Branch(Leaf(n), Leaf(n))).size must_== 5 }
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

  //===== Exercise 3.27 =====
  "depth" should {
    "return 1 when only one node exists" in { Leaf(0).depth must_== 1 }
    "return 2 when only one branch that has two leaves exists" in {
      Branch(Leaf(0), Leaf("foo")).depth must_== 2
    }
    "return 3 when a branch has two level trees" in {
      Branch(Branch(Leaf(0), Leaf(1)), Leaf(2)).depth must_== 3
    }
  }

  //===== Exercise 3.28 =====
  "map" should {
    "return Leaf(2) when (_ + 1) maps over Leaf(1)" in { (Leaf(1) map { _ + 1 }) must_== Leaf(2) }
    "return Branch(Leaf(2), Leaf(4)) when (_ * 1) maps over Branch(Leaf(1), Leaf(2))" in {
      (Branch(Leaf(1), Leaf(2)) map { _ * 2 }) must_== Branch(Leaf(2), Leaf(4))
    }
  }

  //===== Exercise 3.29 =====
  "sizeViaFold" should {
    "return 1 when only one node exists" in { Leaf(0).sizeViaFold must_== 1 }
    "return 3 when a branch has two leaves" in { Branch(Leaf(0), Leaf(0)).sizeViaFold must_== 3 }
    "return 5 when a branch has a leaf and a branch that has two leaves " in {
      Branch(Leaf(0), Branch(Leaf(0), Leaf(0))).sizeViaFold must_== 5
    }
  }
//  "maximumViaFold" should {
//    "return the maximumViaFold element in a Tree[Int]" ! forAll { (a: Int, b: Int, c: Int) =>
//      Branch(Branch(Leaf(a), Leaf(b)), Leaf(c)).maximumViaFold must_== (a max b max c)
//    }
//    "throw exception when a list is not Int" in {
//      Leaf[String]("foo").maximumViaFold must throwA[UnsupportedOperationException]
//    }
//  }
  //"depthByFold" should {
  //  "return 1 when only one node exists" in { Leaf(0).depthByFold must_== 1 }
  //  "return 2 when only one branch that has two leaves exists" in {
  //    Branch(Leaf(0), Leaf("foo")).depthByFold must_== 2
  //  }
  //  "return 3 when a branch has two level trees" in {
  //    Branch(Branch(Leaf(0), Leaf(1)), Leaf(2)).depthByFold must_== 3
  //  }
  //}
  "mapViaFold" should {
    "return Leaf(2) when (_ + 1) mapViaFolds over Leaf(1)" in {
      (Leaf(1) mapViaFold { _ + 1 }) must_== Leaf(2)
    }
    "return Branch(Leaf(2), Leaf(4)) when (_ * 1) mapViaFolds over Branch(Leaf(1), Leaf(2))" in {
      (Branch(Leaf(1), Leaf(2)) mapViaFold { _ * 2 }) must_== Branch(Leaf(2), Leaf(4))
    }
  }
}
