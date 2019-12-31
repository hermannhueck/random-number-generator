package random01

import scala.util.chaining._
import util._

/*
  Mutable generation of pseudo-random values using scala.util.Random
 */
object RandMutable extends App {

  printStartLine()

  s"----- Mutable generation of pseudo-random values with 'scala.util.Random'" pipe println

  import Random._

  println(s"random Int:     ${randomInt(rng)}")
  println(s"random Double:  ${randomDouble(rng)}")
  println(s"random Boolean: ${randomBoolean(rng)}")
  println(s"random String:  ${randomString(12, rng)}")
  println(s"random IntPair: ${randomIntPair(rng)}")

  "----- Rolling dice ..." pipe println

  def rollDie(rng: RNG): Int =
    1 + rng.nextInt(6)

  def rollDieNTimes(n: Int): List[Int] =
    if (n <= 0)
      List.empty[Int]
    else
      (0 until n).toList map (_ => rollDie(rng))

  println(s"Rolled die 20 times: ${rollDieNTimes(20)}")

  printEndLine()
}
