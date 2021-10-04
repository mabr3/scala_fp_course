import scala.annotation.tailrec
import scala.math.abs

object session{

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double = if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess -x) < 1.0e50

    def improve(guess: Double) =
      (guess +x/guess) /2

    sqrtIter(1.0)
  }


  def factorial(n: Int): Int = {
    @tailrec
    def loop(acc: Int, n: Int) :Int =
      if (n==0) acc
      else loop(acc*n, n-1)

    loop(1, n)
  }

}
println(session.sqrt(1e-6))
println(session.sqrt(1e60))
println(session.factorial(3))


