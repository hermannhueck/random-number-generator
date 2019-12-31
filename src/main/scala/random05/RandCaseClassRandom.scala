package random05

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  Functions wrapped in a case class:
  The functions 'RNG => (RNG, Long)' are now wrapped in to a 'case class Random[A](run: RNG => (RNG, Long))'.

  This wrapping gives us a computational context which enables more functional abstractions
  as we will see in the following steps.

  The random value generation functions now return 'Random[A]' and I moved them to the 'Random' companion object.
 */
object RandCaseClassRandom extends App {

  printStartLine()

  s"----- Wrapping the functions 'RNG => (RNG, A)' in case class 'Random'" pipe println

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

  val rollDie: Random[Int] = Random { rng =>
    val (newRng, i) = nonNegativeInt.run(rng)
    (newRng, 1 + i % 6)
  }

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
