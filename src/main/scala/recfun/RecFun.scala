package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  //0             1
  //1            1 1
  //2           1 2 1
  //3          1 3 3 1
  //4         1 4 6 4 1
  //5        1 5 10 10 5 1
  //6       1 6 15 20 15 6 1
  //7      1 7 21 35 35 21 7 1
  //8     1 8 28 56 70 56 28 8 1
  /**
   * Exercise 1
   */
  def pascal2(c: Int, r: Int): Int = {
    def loop(nextRow: Int, last: List[Int]): List[List[Int]] = {
      if (nextRow >= r) List(last)
      else {
        val slices =
          if (last.length == 1) List(1, 1)
          else 1 +: last.sliding(2).map(_.sum).toList :+ 1
        last :: loop(nextRow + 1, slices)
      }
    }

    loop(0, List(1))(r)(c)
  }

  def pascal(c: Int, r: Int): Int = {
    def loop(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else loop(c - 1, r - 1) + loop(c, r - 1)
    }

    loop(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(count: Int, rest: List[Char]): Boolean = (count, rest) match
      case (c, xs) if c < 0 => false
      case (c, Nil) if c == 0 => true
      case (c, Nil) => false
      case (_, h :: t) =>
        if (h == '(') loop(count + 1, t)
        else if (h == ')') loop(count - 1, t)
        else loop(count, t)

    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(restMoney: Int, restCoins: List[Int]): Int = (restMoney, restCoins) match
      case (count, xs) if count < 0 || xs.isEmpty => 0
      case (count, _) if count == 0 => 1
      case (_, h :: t) =>
        loop(restMoney - h, restCoins) + loop(restMoney, t)

    if (money == 0 || coins.isEmpty) return 0
    loop(money, coins)
  }
