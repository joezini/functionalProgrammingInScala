package Chapter4

/**
  * Created by edwardsj on 25/08/2016.
  */
object Maths {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    m.flatMap(mu => mean(xs.map(x => math.pow(x - mu, 2))))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a match {
      case None => None
      case Some(x) => b match {
        case None => None
        case Some(y) => Some(f(x, y))
      }
    }
  }

  def map2a[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.map(y => f(x, y)))
  }
}

object MyMathsApp extends App {

  val firstFour = Seq(1.0,2.0,3.0,4.0)
  val emptyList = Seq()

  println("Mean of [1,2,3,4] = 2.5? => " + Maths.mean(firstFour))
  println("Mean of empty list = None? => " + Maths.mean(emptyList))
  println("Variance of [1,2,3,4] => " + Maths.variance(firstFour))
  println("Map2 with add Some(1) + Some(2) => " + Maths.map2(Some(1), Some(2))(_ + _))
}