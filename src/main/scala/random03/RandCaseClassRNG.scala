package random03

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  This impl step wraps seed generation in 'case class RNG(seed: Long)
  This gives seed generation its own type and makes the client code more robust.

  Still annoying: We have to thread the RNG from one invocation to the next.
 */
object RandCaseClassRNG extends App {

  printStartLine()

  s"----- Wrapping seed generation in case class 'RNG'" pipe println

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

  def rollDie(rng: RNG): (RNG, Int) = {
    val (newRng, i) = nonNegativeInt(rng)
    (newRng, 1 + i % 6)
  }

  def rollDieNTimes(n: Int)(rng: RNG): (RNG, List[Int]) = {
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
