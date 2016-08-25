package Chapter2

/**
  * Created by edwardsj on 09/06/2016.
  */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(x: Int, y: Int, i: Int): Int = {
      if (i == 1) x
      else go(y, x + y, i - 1)
    }

    go(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n == as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n+1)
      else false
    }

    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("number in the fibonacci sequence at the position", 7, fibonacci))
    println(isSorted(Array(1, 2, 3, 3), (x: Int,y: Int) => x < y))
    //println(isSorted(Array(Array(1,2), Array('a', 'g', 't'), Array(34.343, 454.33243, 87687.5, 1232.44)), (x: Array[Any], y: Array[Any]) => x.length <= y.length))
  }
}


