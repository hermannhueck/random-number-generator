package random19

import cats.Applicative

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  In this step we go from Int pairs to generic pairs of type A and B.
  Accordingly we can write generic triples (and other tuples if we like).
  We can also build generic random lists with 'listOf(n: Int)(rand: Random[A])'.
  'oneOf' allows us to randomly select one element of a given list.
  'nOf' combines 'listOf' and 'oneOf' to randomly select n elements of a given list.
 */
object RandGenericCombinators extends App {

  printStartLine()

  s"----- Generic Combinators" pipe println

  import Random._

  import cats.syntax.apply._

  val rand = // program description: doesn't do anything!
    (
      int,
      double,
      boolean,
      pair(int, int),
      triple(int, int, int),
      ints(10),
      doubles(10),
      oneOf((0 until 9) map (_ * 10): _*),
      nOf(20)(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
    ).tupled

  val (newRng, (i, d, b, pi, ti, li, ld, onei, ni)) = rand.run(RNG(42)).value // program invocation

  s"random Int:          $i" pipe println
  s"random Double:       $d" pipe println
  s"random Boolean:      $b" pipe println
  s"random IntPair:      $pi" pipe println
  s"random IntTriple:    $ti" pipe println
  s"random IntList:      $li" pipe println
  s"random DoubleList:   $ld" pipe println
  s"random oneOf(...):   $onei" pipe println
  s"random nOf(20)(...): $ni" pipe println

  "----- Rolling dice ..." pipe println

  val rollDie: Random[Int] =
    intFromTo(1, 6)

  def rollDieNTimes(n: Int): Random[List[Int]] =
    listOf(n)(rollDie)

  val rolled = rollDieNTimes(20).runA(newRng).value
  s"Rolled die 20 times: $rolled" pipe println

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
