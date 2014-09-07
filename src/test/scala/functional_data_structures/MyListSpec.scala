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

  "dropWhile" should {
    "remove elements from a list prefix as long as they match a predicate" ! forAll {
      (xs: List[Int], f: Int => Boolean) => MyList.dropWhile(xs, f) must_== xs.dropWhile(f)
    }
  }

  "append" should {
    "add one list to the end of another" ! prop {
      (xs1: List[Int], xs2: List[Int]) => MyList.append(xs1, xs2) must_== xs1 ++ xs2
    }
  }

  "init" should {
    "returns a List consisting of all but the last element of a List" ! forAll {
      xs: List[Int] => xs.nonEmpty ==> (MyList.init(xs) == xs.init)
    }
  }
}
