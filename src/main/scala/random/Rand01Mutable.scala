package random

/*
  Mutable generation of pseudo-random values using scala.util.Random
 */
object Rand01Mutable extends App {

  println("\n----- Mutable generation of pseudo-random values with 'scala.util.Random'")

  type RNG = scala.util.Random

  val rng: RNG = new scala.util.Random(42)

  def randomInt(rng: RNG): Int = rng.nextInt

  def randomDouble(rng: RNG): Double = rng.nextDouble

  def randomBoolean(rng: RNG): Boolean = rng.nextBoolean

  def randomString(length: Int, rng: RNG): String = rng.nextString(length)

  def randomIntPair(rng: RNG): (Int, Int) =
    (randomInt(rng), randomInt(rng))


  println("random Int:     " + randomInt(rng))
  println("random Double:  " + randomDouble(rng))
  println("random Boolean: " + randomBoolean(rng))
  println("random String:  " + randomString(12, rng))
  println("random IntPair: " + randomIntPair(rng))


  println("----- Rolling dies ...")

  def rollDie(rng: RNG): Int = {
    1 + rng.nextInt(6)
  }

  def rollDieNTimes(n: Int): List[Int] =
    if (n <= 0)
      List.empty[Int]
    else
      (0 until n).toList map (_ => rollDie(rng))

  println("Rolled die 20 times: " + rollDieNTimes(20))

  println("-----\n")
}
