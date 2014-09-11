package exceptions

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Prop.forAll

class StatSpec extends Specification with ScalaCheck {
  "variance" should {
    "return the variance of a list" ! forAll { xs: Seq[Double] =>
        Stat.variance(xs) must_==
            Stat.mean(xs map ((x: Double) => math.pow(x - Stat.mean(xs).getOrElse(0.0), 2.0)))
    }
  }
}
