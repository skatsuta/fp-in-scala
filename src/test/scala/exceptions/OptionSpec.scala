package exceptions

import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class OptionSpec extends Specification with ScalaCheck {
  "map" should {
    "return None when applied to None" in { (None map { (_: Int) + 1 }) must_== None }
    "return an option value mapped over f if the value exists" ! forAll { n: Int =>
      (Some(n) map { _ + 1 }) must_== Some(n + 1)
    }
  }

  //"flatMap" should {
  //  "return None when applied to None" in { (None flatMap { _ find (x => x % 2 == 0) }) must_== None }
  //}

  "getOrElse" should {
    "return the default value when applied to None" ! forAll { default: String =>
      (None getOrElse default) must_== default
    }
    "return a value when it has a value" ! forAll { (value: String, default: String) =>
      (Some(value) getOrElse default) must_== value
    }
  }

  "orElse" should {
    "return the default option value when applied to None" ! forAll { ob: Int => {
        (None orElse None) must_== None
        (None orElse Some(ob)) must_== Some(ob)
      }
    }
    "return the argument itself when applied to Some" ! forAll { (x: Double, ob: Double) => {
        (Some(x) orElse None) must_== Some(x)
        (Some(x) orElse Some(ob)) must_== Some(x)
      }
    }
  }

  "filter" should {
    "return None when applied to None" in { None.filter { (_: Int) % 2 == 0 } must_== None }
    "return the argument itself if there exists a value and otherwise return None" ! forAll { n: Int =>
      Some(n).filter { (_: Int) % 2 == 0 } must_== (if (n % 2 == 0) Some(n) else None)
    }
  }

  "lift2" should {
    def add: Int => Int => Int = x => y => x + y

    "be an instance of [Int => Int => Int]" ! forAll { a: Int =>
      Some(a).lift2(add) must beAnInstanceOf[Int => Int => Int]
    }
    "return None when the receiver is None" ! forAll { b: Int => None.lift2(add)(Some(b)) must_== None }
    "return None when applied to None" ! forAll { a: Int => Some(a).lift2(add)(None) must_== None }
    "combine two Option values using a binary function" ! forAll { (a: Int, b: Int) =>
      Some(a).lift2(add)(Some(b)) must_== Some(add(a)(b))
    }
  }
}

