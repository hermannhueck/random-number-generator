package random07

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  Implementing 'flatMap' opens up the possibility to improve 'nextIntPair'.
  This also allows us to implement 'rollDieNTimes2' with a for-comprehension.

  With 'map' and 'flatMap' we can mostly avoid threading the RNG through
  the functions or through the program.
 */
object RandFlatMap extends App {

  printStartLine()

  s"----- Introducing 'flatMap'" pipe println

  import Random._

  val rand: Random[(Int, Double, Boolean, (Int, Int))] = for { // program description: doesn't do anything!
    i  <- int
    d  <- double
    b  <- boolean
    ip <- intPair
  } yield (i, d, b, ip)

  val (newRng, (i, d, b, ip)) = rand.run(RNG(42)) // program invocation

  s"random Int:     $i" pipe println
  s"random Double:  $d" pipe println
  s"random Boolean: $b" pipe println
  s"random IntPair: $ip" pipe println

  "----- Rolling dice ..." pipe println

  val rollDie: Random[Int] =
    nonNegativeInt map (i => 1 + i % 6)

  def rollDieNTimes1(n: Int): Random[List[Int]] =
    if (n <= 0)
      Random(rng => (rng, List.empty[Int]))
    else
      Random { rng =>
        val (r1, x)  = rollDie.run(rng)
        val (r2, xs) = rollDieNTimes1(n - 1).run(r1)
        (r2, x :: xs)
      }

  def rollDieNTimes2(n: Int): Random[List[Int]] =
    if (n <= 0)
      Random(rng => (rng, List.empty[Int]))
    else
      for {
        x  <- rollDie
        xs <- rollDieNTimes2(n - 1)
      } yield x :: xs

  val rolled: Random[(List[Int], List[Int])] = for { // program description: doesn't do anything!
    rolled1 <- rollDieNTimes1(20)
    rolled2 <- rollDieNTimes2(20)
  } yield (rolled1, rolled2)

  val (rolled1, rolled2) = rolled.runA(newRng) // program invocation

  "1. rollDieNTimes: recursive solution" pipe println
  s"Rolled die 20 times: $rolled1" pipe println
  "2. rollDieNTimes: solution using for-comprehension" pipe println
  s"Rolled die 20 times: $rolled2" pipe println

  "----- Random with flatMap (enables for-comnprehensions for Random) ..." pipe println

  def sumOfSquares(ri1: Random[Int], ri2: Random[Int]): Random[Int] =
    for {
      i1 <- ri1
      i2 <- ri2
    } yield i1 * i1 + i2 * i2

  private val random = sumOfSquares(rollDie, rollDie)
  val randomResult   = random.runA(RNG(42))
  s"sumOfSquares[Random]: $randomResult" pipe println

  printEndLine()
}
