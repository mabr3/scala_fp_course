package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val a = union(singletonSet(2), singletonSet(3))
  print(printSet(singletonSet(2)))
  println(contains(a,2))
  println(contains(a,3))

}
