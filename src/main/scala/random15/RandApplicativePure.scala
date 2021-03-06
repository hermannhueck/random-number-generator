package random15

import cats.Applicative

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  'pure' is already defined in Cats Applicative.
  Hence we can delete our impl of 'pure' and use 'Applicative.pure' instead.
 */
object RandApplicativePure extends App {

  printStartLine()

  s"----- Replacing 'Random.pure' with Cats 'Applicative.pure'" pipe println

  import Random._

  import cats.syntax.apply._

  val rand: Random[(Int, Double, Boolean, (Int, Int))] = // program description: doesn't do anything!
    (int, double, boolean, intPair).tupled

  val (newRng, (i, d, b, ip)) = rand.run(RNG(42)).value // program invocation

  s"random Int:     $i" pipe println
  s"random Double:  $d" pipe println
  s"random Boolean: $b" pipe println
  s"random IntPair: $ip" pipe println

  "----- Rolling dice ..." pipe println

  val rollDie: Random[Int] =
    nonNegativeInt map (i => 1 + i % 6)

  def rollDieNTimes1(n: Int): Random[List[Int]] =
    if (n <= 0)
      Applicative[Random].pure(List.empty[Int]) // Applicative.pure
    else
      Random { rng =>
        val (r1, x)  = rollDie.run(rng).value
        val (r2, xs) = rollDieNTimes1(n - 1).run(r1).value
        (r2, x :: xs)
      }

  import cats.syntax.applicative._

  def rollDieNTimes2(n: Int): Random[List[Int]] =
    if (n <= 0)
      List.empty[Int].pure[Random] // Applicative.pure with extension method for List
    else
      (rollDie, rollDieNTimes2(n - 1)) mapN (_ :: _)

  val rolled: Random[(List[Int], List[Int])] = // program description: doesn't do anything!
    (rollDieNTimes1(20), rollDieNTimes2(20)).tupled

  val (rolled1, rolled2) = rolled.runA(newRng).value // program invocation

  "1. rollDieNTimes: recursive solution" pipe println
  s"Rolled die 20 times: $rolled1" pipe println
  "2. rollDieNTimes: solution using mapN" pipe println
  s"Rolled die 20 times: $rolled2" pipe println

  "----- Applicative Random ..." pipe println

  def sumOfSquares[F[_]: Applicative](mi1: F[Int], mi2: F[Int]): F[Int] =
    (mi1, mi2) mapN ((i1, i2) => i1 * i1 + i2 * i2)

  import cats.instances.option._

  private val optionResult: Option[Int] = sumOfSquares(Option(3), Option(4))
  s"sumOfSquares[Option[Int]]: $optionResult" pipe println

  private val random: Random[Int] = sumOfSquares(rollDie, rollDie)
  val randomResult                = random.runA(RNG(42)).value
  s"sumOfSquares[Random[Int]]: $randomResult" pipe println

  printEndLine()
}
