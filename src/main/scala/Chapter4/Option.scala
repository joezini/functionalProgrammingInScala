package Chapter4

/**
  * Created by edwardsj on 18/08/2016.
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](backup: => B): B = this match {
    case None => backup
    case Some(x) => x
  }

  //  def orElse[B >: A](backup: Option[B]): Option[B] = this match {
  //    case None => backup
  //    case _ => _
  //  }
  //
  //  def flatMap[B](f: A => Option[B]): Option[B] = this match {
  //    case None => None
  //    case Some(x) => f(x)
  //  }
  //
  //  def filter(f: A => Boolean): Option[A] = this match {
  //    case Some(x) if f(x) => Some(x)
  //    case _ => None
  //  }
  def filter(f: A => Boolean): Option[A] = {
    if(map(f).getOrElse(false)) this else None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def orElse[B >: A](backup: Option[B]): Option[B] = {
    map(x => Some(x)) getOrElse backup
  }

}

case object None extends Option[Nothing]      // The Nothing is a subclass of everything so
case class Some[+A](get: A) extends Option[A]

object MyApp extends App {
  val none = None
  val someInt = Some(5)
  val someFour = Some(4)

  def triple(x: Int): Int = {
    3 * x
  }

  def isFive(x: Int): Boolean = {
    x == 5
  }

  println("Test map: expect Some(15) => " + someInt.map(triple))
  println("Test filter: expect Some(5) => " + someInt.filter(isFive))
  println("Test filter: expect None => " + someFour.filter(isFive))
}

