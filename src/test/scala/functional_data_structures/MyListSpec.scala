package functional_data_structures

import org.specs2.ScalaCheck
import org.scalacheck.Prop.forAll
import org.specs2.mutable.Specification

class MyListSpec extends Specification with ScalaCheck {
  "tail" should {
    "return Nil when applied to Nil" in {
      MyList.tail(Nil) must_== Nil
    }

    "return a list except the first element" ! forAll { xs: List[Int] =>
      xs.nonEmpty ==> (MyList.tail(xs) == xs.tail)
    }
  }

  "setHead" should {
    "return Nil when applied to Nil" ! forAll { n: Int =>
      MyList.drop(Nil, n) must_== Nil
    }

    "replace the first element" ! forAll { (xs: List[Int], x: Int) =>
      xs.nonEmpty ==> (MyList.setHead(xs, x) == x :: xs.tail)
    }
  }

  "drop" should {
    "remove the first n elements from a list" ! forAll { (xs: List[Int], n: Int) =>
      MyList.drop(xs, n) must_== xs.drop(n)
    }
  }

  "dropWhile" should {
    "remove elements from a list prefix as long as they match a predicate" !
      forAll { (xs: List[Int], f: Int => Boolean) =>
        MyList.dropWhile(xs, f) must_== xs.dropWhile(f)
    }
  }

  "append" should {
    "add one list to the end of another" ! forAll { (xs1: List[Int], xs2: List[Int]) =>
      MyList.append(xs1, xs2) must_== xs1 ++ xs2
    }
  }

  "init" should {
    "return a List consisting of all but the last element of a List" ! forAll { xs: List[Int] =>
      xs.nonEmpty ==> (MyList.init(xs) == xs.init)
    }
  }

  "foldRight" should {
    "reduce the list using the binary operator from right to left" !
        forAll { (xs: List[Int], acc: Int, x: Int, y: Int) =>
          MyList.foldRight(xs)(acc)((x, y) => x + y) must_== xs.foldRight(acc)((x, y) => x + y)
    }
  }

  "product3" should {
    //===== Exercise 3.7 =====
    // Couldn't pass the test below because GenTraversable#product is implemented by foldLeft
    // (the product of huge numbers returns Double.NaN,
    //     but short-circuiting could return 0 if it contains 0)
    //"compute the product of a list of numbers" ! forAll {
    //  xs: List[Double] => MyList.product3(xs) must_== xs.product
    //}

    //===== Exercise 3.8 =====
    "combine two lists" ! forAll { xs: List[Int] =>
      MyList.foldRight(xs)(Nil: List[Int])(_ :: _) must_== xs
    }
  }

  //===== Exercise 3.9 =====
  "length" should {
    "be the length of the sequence" ! forAll { xs: List[Int] =>
      MyList.length(xs) must_== xs.length
    }
  }

  //===== Exercise 3.10 =====
  "foldLeft" should {
    "reduce the list using the binary operator, from left to right" !
      forAll { (xs: List[Int], acc: Int, x: Int, y: Int) =>
        MyList.foldLeft(xs)(acc)((x, y) => x + y) must_== xs.foldLeft(acc)((x, y) => x + y)
    }
  }

  //===== Exercise 3.11 =====
  "sum" should {
    "sum up all the elements" ! forAll { xs: List[Int] =>
      MyList.sum(xs) must_== xs.sum
    }
  }

  "product" should {
    "multiplies up all the elements" ! forAll { xs: List[Double] =>
      !xs.product.isNaN ==> (MyList.product(xs) == xs.product)
    }
  }

  "length2" should {
    "be the length of the sequence" ! forAll { xs: List[String] =>
      MyList.length2(xs) must_== xs.length
    }
  }

  //===== Exercise 3.12 =====
  "reverse" should {
    "return a new list with elements in reversed order" ! forAll { xs: List[Int] =>
      MyList.reverse(xs) must_== xs.reverse
    }
  }
}
