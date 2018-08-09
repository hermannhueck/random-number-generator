package random

/*
  Mutable generation of pseudo-random values using scala.util.Random
 */
object Rand01Mutable extends App {

  println("\n----- Mutable generation of pseudo-random values with 'scala.util.Random'")

  val rng = new scala.util.Random(42)

  println(  "random Int:     " + rng.nextInt  )
  println(  "random Double:  " + rng.nextDouble  )
  println(  "random Boolean: " + rng.nextBoolean  )
  println(  "random String:  " + rng.nextString(12)  )

  def randomIntPair: (Int, Int) = {
    val rng = new scala.util.Random(System.currentTimeMillis)
    (rng.nextInt, rng.nextInt)
  }

  println(  "random IntPair: " + randomIntPair  )

  def rollDie: Int = {
    val rng = new scala.util.Random
    1 + rng.nextInt(6)
  }

  def rollDieNTimes(times: Int): List[Int] =
    if (times <= 0)
      List.empty[Int]
    else
      (0 until times).toList map (_ => rollDie)

  println(  "Rolled die 20 times: " + rollDieNTimes(20)  )

  println("-----\n")
}
