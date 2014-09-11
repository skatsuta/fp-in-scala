package exceptions

object Stat {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap { m => mean(xs map { x => math.pow(x - m, 2.0) }) }

    // 最初の実装
    //val m = mean(xs).getOrElse(0.0)
    //val diffSquares = xs map { x => math.pow(x - m, 2.0) }
    //mean(diffSquares)
  }
}
