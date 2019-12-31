package random20

import cats.Applicative

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  This step adds random strings of various kinds.
 */
object RandStringCombinators extends App {

  printStartLine()

  s"----- String Combinators" pipe println

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
      nOf(20)(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
      chars(20),
      string(20),
      ansiString(20),
      asciiString(20),
      alphaNumString(80),
      alphaString(20),
      numericString(20)
    ).tupled

  val (newRng, (i, d, b, pi, ti, li, ld, onei, ni, cs, s, ansi, ascii, alphaNum, alpha, num)) =
    rand.run(RNG(42)).value // program invocation

  s"random Int:          $i" pipe println
  s"random Double:       $d" pipe println
  s"random Boolean:      $b" pipe println
  s"random IntPair:      $pi" pipe println
  s"random IntTriple:    $ti" pipe println
  s"random IntList:      $li" pipe println
  s"random DoubleList:   $ld" pipe println
  s"random oneOf(...):   $onei" pipe println
  s"random nOf(20)(...): $ni" pipe println
  s"random chars:        $cs" pipe println
  s"random String:       $s" pipe println
  s"random ANSI String:  $ansi" pipe println
  s"random ASCII String: $ascii" pipe println
  s"random alphaNum String: $alphaNum" pipe println
  s"random alpha String: $alpha" pipe println
  s"random numeric String: $num" pipe println

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
