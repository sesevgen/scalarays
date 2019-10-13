

object Change extends App{
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money < 0) return 0
    else if (money == 0) return 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
  println(countChange(210, List(2,3,5)))
}