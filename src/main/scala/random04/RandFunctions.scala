package random04

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  Methods to Functions:
  Every random value generation method is refactored to a function of type: RNG => (RNG, Long)
  Most def's can be turned into val's.
 */
object RandFunctions extends App {

  printStartLine()

  s"----- Moving from methods to functions 'RNG => (RNG, A)'" pipe println

  import Random._

  val rng0       = RNG(42)
  val (rng1, i)  = randomInt(rng0)
  val (rng2, d)  = randomDouble(rng1)
  val (rng3, b)  = randomBoolean(rng2)
  val (rng4, ip) = randomIntPair(rng3)

  s"random Int:     $i" pipe println
  s"random Double:  $d" pipe println
  s"random Boolean: $b" pipe println
  s"random IntPair: $ip" pipe println

  "----- Rolling dice ..." pipe println

  val rollDie: RNG => (RNG, Int) = rng => {
    val (newRng, i) = nonNegativeInt(rng)
    (newRng, 1 + i % 6)
  }

  def rollDieNTimes(n: Int): RNG => (RNG, List[Int]) = rng => {
    if (n <= 0)
      (rng, List.empty[Int])
    else {
      val (r1, x)  = rollDie(rng)
      val (r2, xs) = rollDieNTimes(n - 1)(r1)
      (r2, x :: xs)
    }
  }

  val (rng5, rolled) = rollDieNTimes(20)(rng4)
  s"Rolled die 20 times: $rolled" pipe println

  printEndLine()
}
