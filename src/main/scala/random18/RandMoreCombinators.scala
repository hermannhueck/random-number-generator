package random18

import cats.Applicative
import cats.instances.list._
import cats.syntax.traverse._

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  Having constucted the basic functional structure of random value generation
  we can develop more combinators based on the existing ones.
 */
object RandMoreCombinators extends App {

  printStartLine()

  s"----- More Combinators" pipe println

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
    intFromTo(1, 6)

  def rollDieNTimes(n: Int): Random[List[Int]] =
    (0 until (0 max n)).toList traverse (_ => rollDie)

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
