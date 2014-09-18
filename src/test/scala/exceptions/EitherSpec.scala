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
      Left(str).flatMap(x => Right(x)) must_== Left(str)
    }
    "return Right if applied to Right" ! forAll { x: Int =>
      Right(x).flatMap(x => Right(x)) must_== Right(x)
    }
  }
}
