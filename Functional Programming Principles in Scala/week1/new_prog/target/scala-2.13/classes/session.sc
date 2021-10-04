import scala.math.abs

object session extends App {
  val x= 1+2

  def sqrtIter(guess: Double, x: Double): Double = if (isGoodEnough(guess,x)) guess
    else sqrtIter(improve(guess,x),x)

  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess -x) < 0.001

  def improve(guess: Double, x: Double) =
    (guess +x/guess) /2

  def sqrt(x: Double) = sqrtIter(1.0 , x)

}
println(session.sqrt(1e-6))
println(session.sqrt(1e60))


