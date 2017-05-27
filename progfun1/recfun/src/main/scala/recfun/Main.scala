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

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c <= 0) 1
    else if (r <= 0 || r == c) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = chars.foldLeft(0)((a, c) => c match {
      case ')' => if (a >= 0) a - 1 else -1
      case '(' => if (a >= 0) a + 1 else -1
      case _ => a
    }) == 0

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    money match {
      case 0 => 1
      case x if x < 0 => 0
      case _ =>
        coins match {
          case Nil => 0
          case head +: tail =>
            countChange(money - head, coins) + countChange(money, tail)
        }
    }
  }
}
