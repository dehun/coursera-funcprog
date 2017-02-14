package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  //  1
  //  1 1
  //  1 2 1
  //  1 3 3 1

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = r match {
    case 0 => 1
    case _ => {
      val pr_width = r
      val left = if (c > 0) pascal(c - 1, r - 1) else 0
      val right = if (c < pr_width) pascal(c, r - 1) else 0
      return left + right
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val brackets = chars.filter(c => Set('(', ')').contains(c))
    var balanced = 0
    for {b <- brackets} {
      balanced += (b match {
        case '(' => 1
        case ')' => -1
      })
      if (balanced < 0) return false
    }
    return balanced == 0
  }

  /**
    * Exercise 3
    */
  // 1 2 = 4
  // 1 + 1 + 1 + 1
  // 1 + 1 + 2
  // 2 + 2
  def innerCountChange(money: Int, coins: List[Int], path:List[(Int, Int)]): Int = {
    if (money == 0) {
      //Console.println(path)
      return 1
    }

    val validCoins = coins.filter(_ <= money)
    if (validCoins.isEmpty) return 0
    var variants = 0
    val coin = validCoins.head
    for {times <- List.range(0, money/coin + 1)} {
      //Console.println(s"coin=${coin} x ${times} for money = ${money - coin * times}")
      variants += innerCountChange(money - coin * times, validCoins.tail, (coin, times)::path)
    }

    variants
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    innerCountChange(money, coins, List())
  }



  def countChange2(amoney: Int, acoins: List[Int]): Int = {
    def innerCountChange(money: Int, coins: List[Int], path: List[Int]) : Int = money match {
      case 0 => {
        1
      }
      case _ => {
        val lastCoinLimit = if (path.isEmpty) money else path.head
        val nextCoins = coins.takeWhile(_ <= lastCoinLimit)
        nextCoins.fold(0)((acc: Int, coin: Int) => {
          acc + innerCountChange(money - coin, nextCoins, coin :: path)
        })
      }
    }

    innerCountChange(amoney, acoins, List())
  }
}
