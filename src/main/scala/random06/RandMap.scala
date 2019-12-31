package random06

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  Now we implement 'map' on our 'case class Random'.
  Using 'map' we can simplify many of our random value generation functions.
 */
object RandMap extends App {

  printStartLine()

  s"----- Introducing 'map'" pipe println

  import Random._

  val rng0       = RNG(42)
  val (rng1, i)  = int.run(rng0)
  val (rng2, d)  = double.run(rng1)
  val (rng3, b)  = boolean.run(rng2)
  val (rng4, ip) = intPair.run(rng3)

  s"random Int:     $i" pipe println
  s"random Double:  $d" pipe println
  s"random Boolean: $b" pipe println
  s"random IntPair: $ip" pipe println

  "----- Rolling dice ..." pipe println

  val rollDie: Random[Int] =
    nonNegativeInt map (i => 1 + i % 6)

  def rollDieNTimes(n: Int): Random[List[Int]] = Random { rng =>
    if (n <= 0)
      (rng, List.empty[Int])
    else {
      val (r1, x)  = rollDie.run(rng)
      val (r2, xs) = rollDieNTimes(n - 1).run(r1)
      (r2, x :: xs)
    }
  }

  val (rng5, rolled) = rollDieNTimes(20).run(rng4)
  s"Rolled die 20 times: $rolled" pipe println

  printEndLine()
}
