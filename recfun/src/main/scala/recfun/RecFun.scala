package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    def factorial(number: Int, acc: Int = 1): BigInt = {
      if number == 0 then acc else factorial(number - 1, acc * number)
    }
    (factorial(r)/(factorial(c) * factorial(r - c))).intValue


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def iterator(remain: List[Char], opened_braces: Int): Boolean = {
      def symToDigit(char: Char): Int = char match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
      if opened_braces < 0 then false
      else if remain.isEmpty then opened_braces == 0
      else iterator(remain.tail, opened_braces + symToDigit(remain.head))
    }
    iterator(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def helper(remainMoney: Int, remainCoins: List[Int], acc: Int): Int =
      if remainMoney == 0 then acc + 1
      else if remainMoney < 0 || remainCoins.isEmpty then acc
      else helper(remainMoney - remainCoins.head, remainCoins, acc + countChange(remainMoney, remainCoins.tail))
    helper(money, coins, 0)