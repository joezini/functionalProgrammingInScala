package Cafe

/**
  * Created by edwardsj on 26/05/2016.
  */
class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1 combine c2))
  }
}

class CreditCard(ccname: String) {
  val name = ccname
}

class Coffee {
  val price = 2.40
  val name = "Skinny moccha soy lattechino"
}

case class Charge(cc: CreditCard, amt: BigDecimal) {
  def combine(other: Charge): Charge =
    if(cc == other.cc)
      Charge(cc, amt + other.amt)
    else
      throw new Exception("Can't combine charges to different cards")
}

object myProgram extends App {
  val myCafe = new Cafe
  val myCard = new CreditCard("Joe")
  val (coffee, charge): (Coffee, Charge) = myCafe.buyCoffee(myCard)

  println(s"${coffee.name} costs £${charge.amt} which is charged to ${charge.cc.name}'s card")

  val (coffee2, charge2): (Coffee, Charge) = myCafe.buyCoffee(myCard)
  val combinedCharge = charge.combine(charge2)

  println(s"2 coffees cost £${combinedCharge.amt} which is charged to ${combinedCharge.cc.name}'s card")

  val (coffees, coffeesCharge) = myCafe.buyCoffees(myCard, 5)
  println(s"I just bought ${coffees.length} coffees for £${coffeesCharge.amt}")

  val x = "Hello, World"
  println(s"r1 = ${"Hello, World".reverse}")
  println(s"r2 = ${"Hello, World".reverse}")

  val y = new StringBuilder("Hello")
  println(s"r1 = ${y.append(", World")}")
  println(s"r2 = ${y.append(", World")}")
}