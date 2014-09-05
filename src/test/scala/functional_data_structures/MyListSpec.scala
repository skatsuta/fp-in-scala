package functional_data_structures

import org.specs2.ScalaCheck
import org.scalacheck.Prop.forAll
import org.specs2.mutable.Specification

class MyListSpec extends Specification with ScalaCheck {
  "tail" should {
    "return Nil when applied to Nil" in {
      MyList.tail(Nil) must_== Nil
    }

    "return a list except the first element" ! forAll {
      xs: List[Int] => xs.nonEmpty ==> (MyList.tail(xs) == xs.tail)
    }
  }

  "setHead" should {
    "return Nil when applied to Nil" ! forAll {
      n: Int => MyList.drop(Nil, n) must_== Nil
    }

    "replace the first element" ! forAll {
      (xs: List[Int], x: Int) => xs.nonEmpty ==> (MyList.setHead(xs, x) == x :: xs.tail)
    }
  }

  "drop" should {
    "remove the first n elements from a list" ! prop {
      (xs: List[Int], n: Int) => MyList.drop(xs, n) must_== xs.drop(n)
    }
  }
}
