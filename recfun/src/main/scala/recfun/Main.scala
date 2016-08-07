package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Balance")
    println(balance("())(".toList))

    println("Count change")
    println(countChange(4, List(1, 2)))
  }

  /**
   * Exercise 1
   */
    def pascal(column: Int, row: Int): Int = {
      if (column == 0 || column >= row) 1
      else pascal(column - 1, row - 1) + pascal(column, row - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def checkBalance(openParenthesis: Int, closingParenthesis: Int, chars: List[Char]): Boolean = {
        if (closingParenthesis > openParenthesis) false
        else {
          if (chars.nonEmpty) {
            if (chars.head == '(') checkBalance(openParenthesis + 1, closingParenthesis, chars.tail)
            else {
              if (chars.head == ')') checkBalance(openParenthesis, closingParenthesis + 1, chars.tail)
              else checkBalance(openParenthesis, closingParenthesis, chars.tail)
            }
          } else {
            if (closingParenthesis < openParenthesis) false
            else true
          }
        }
      }

      checkBalance(0, 0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if(money > 0 && coins.nonEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else 0
    }
  }
