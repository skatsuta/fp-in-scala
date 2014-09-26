package exceptions

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Prop.forAll

class EitherSpec extends Specification with ScalaCheck {
  "map" should {
    def double(x: Int): Int = x * 2

    "return Left if applied to Left" ! forAll { str: String =>
      Left("error").map(double) must_== Left("error")
    }
    "map the Right value applying to the value" ! forAll { x: Int =>
      Right(x).map(double) must_== Right(x * 2)
    }
  }

  "flatMap" should {
    "return Left if applied to Left" ! forAll { str: String =>
      Left(str).flatMap(Right(_)) must_== Left(str)
    }
    "return Right if applied to Right" ! forAll { x: Int =>
      Right(x).flatMap(Right(_)) must_== Right(x)
    }
  }

  "orElse" should {
    "return the argument itself if applied to Left" ! forAll { (str: String, x: Int) =>
      Left(str).orElse(Right(x)) must_== Right(x)
    }
    "return Right if applied to Right" ! forAll { (str: String, x: Int) =>
      Right(x).orElse(Left(str)) must_== Right(x)
    }
  }

  "map2" should {
    def add(a: Int, b: Int) = a + b
    "return Left if the receiver is Left" ! forAll { (str: String, x: Int, y: Int) =>
      Left(str).map2(Right(x)) { add(_, _) } must_== Left(str)
    }
    "return Left if the first argument is Left" ! forAll { (str: String, x: Int) =>
      Right(x).map2(Left(str)) { add(_, _) } must_== Left(str)
    }
    "return the f-applied value wrapped by Right if both Right" ! forAll { (x: Int, y: Int) =>
      Right(x).map2(Right(y)) { add(_, _) } must_== Right(add(x, y))
    }
  }
}
