package recfun

import scala.annotation.tailrec
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.convert.ImplicitConversions.`collection asJava`

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
  /**  for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
   **/
      //println(balance("()".toList))
    println(countChange(4,List(1,2)))
  //  }
  }

  /**
   * Exercise 1
   */

  def pascal(c: Int, r: Int): Int = {
    if (r==0 || c==0 || c==r) 1
     else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(chars : List[Char], acc : Int) : Boolean = {
      if (chars.size == 1) {
        if ((acc == 0 && chars.head != ')' && chars.head != '(') ||
          (acc == 1 && chars.head == ')')) true
        else false
      }
      else {
        if (acc == 0 && chars.head == ')') false
        else
          if (chars.head == '(') loop( chars.tail, acc+1)
          else
            if (chars.head == ')') loop( chars.tail, acc-1)
            else loop(chars.tail, acc)
      }
    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else {
      if (money <0 || coins.size == 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
