package DataStructures

/**
  * Created by edwardsj on 30/06/2016.
  */

sealed trait List[+A]     // +A means covariant classes i.e. A and its subtypes (Animal & Cat, Dog etc.)
                          // Sealed means it can't be extended by classes that inherit from it
                          // - it helps the compiler work out if

case object Nil extends List[Nothing] {

}

case class Cons[+A](h: A, t: List[A]) extends List[A] {

}

object List {
  def apply[A](as: A*): List[A] = {    // A* means an unknown number of parameters of type A
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(j: List[Int]): Int = j match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
}

  def product(p: List[Int]): Int = p match {
    case Nil => 1
    case Cons(h, t) => h * product(t)
  }

  def tail[A](x: List[A]): List[A] = x match {
    case Nil => throw new Exception("Nil has no tail")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](i: A,  x: List[A]): List[A] = x match {
    case Cons(x, xs) => Cons(i, xs)
  }

  def drop[A](n: Int, x: List[A]): List[A] = n match {
    case 0 => x
    case i => drop(i-1, List.tail(x))
  }

  def tailViaDrop[A](x: List[A]) = drop(1, x)

  def dropWhile[A](p: A => Boolean, m: List[A]): List[A] = m match {
    case Nil => Nil
    case Cons(x, xs) => if (p(x)) List.dropWhile(p, xs) else m
  }

  def init[A](x: List[A]): List[A] = x match {
    case Cons(n, Nil) => Nil
    case Cons(n, ns) => Cons(n, init(ns))
  }


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, List.foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = List.foldRight(as, 0)((x,y) => 1 + y)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => List.foldLeft(xs, f(z, x))(f)
    }

  def foldLeftProduct(p: List[Int]): Int = List.foldLeft(p,1)(_ * _)

  def foldLeftLength[A](as: List[A]): Int = List.foldLeft(as, 0)((x,y) => 1 + x)

  def reverse[A](as: List[A]): List[A] = List.foldLeft(as, List[A]())((xs,x) => Cons(x, xs))
}

object myApp extends App {
  println(Cons(1, Cons("asdasf", Nil)))
  println(List(1, "asdaaf"))
  println(List.sum(List(1,2,3,4,5)))
  println(List.product(List(1,2,3,4)))
  println(List.tail(List("apple","banana","canteloupe")))
//  println(List.tail(Nil))
  println(List.setHead(1, List(2,4,5)))
  println(List.drop(2, List(1,2,3,4,5,6)))
  println(List.tail(List(1,2,3)))
  println(List.dropWhile((x: Int) => x < 3, List(1,2,3,4,5,6)))
  println(List.init(List(1,2,3,4)))
  println(List.length(List(1,2,3)))
  println(List.foldRight(List(2,4,6),0)(_ + _))
  println(List.foldLeft(List(2,4,6),0)(_ + _))
  println(List.foldLeftProduct(List(2,4,6)))
  println(List.foldLeftLength(List(2,4,6)))
  println(List.reverse(List(1,2,3,4)))
}