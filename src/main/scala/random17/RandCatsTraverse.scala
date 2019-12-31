package random17

import cats.{Applicative, Traverse}

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  Using typeclass 'cats.Traverse'

  'cats.Traverse' provides traversing and sequencing which I implemented myself.
  In this step I replace the usage of my own impl
  with the usage of 'cats.Traverse.sequence' and 'cats.Traverse.traverse'.
 */
object RandCatsTraverse extends App {

  printStartLine()

  s"----- Using Cats 'Traverse.sequence' and 'Traverse.traverse'" pipe println

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

  import cats.instances.list._
  import cats.syntax.traverse._

  def rollDieNTimes1(n: Int): Random[List[Int]] =
    Traverse[List].sequence((0 until (0 max n)).toList map (_ => rollDie))

  def rollDieNTimes2(n: Int): Random[List[Int]] =
    (0 until (0 max n)).toList.map(_ => rollDie).sequence

  def rollDieNTimes3(n: Int): Random[List[Int]] =
    Traverse[List].traverse((0 until (0 max n)).toList)(_ => rollDie)

  def rollDieNTimes4(n: Int): Random[List[Int]] =
    (0 until (0 max n)).toList traverse (_ => rollDie)

  val rolled = // program description: doesn't do anything!
    (
      rollDieNTimes1(20),
      rollDieNTimes2(20),
      rollDieNTimes3(20),
      rollDieNTimes4(20)
    ).tupled

  val (rolled1, rolled2, rolled3, rolled4) = rolled.runA(newRng).value // program invocation

  s"1. rollDieNTimes: Cats Traverse[List].sequence" pipe println
  println("Rolled die 20 times: " + rolled1)
  s"2. rollDieNTimes: Cats sequence syntax" pipe println
  println("Rolled die 20 times: " + rolled2)
  s"3. rollDieNTimes: Cats Traverse[List].traverse" pipe println
  println("Rolled die 20 times: " + rolled3)
  s"4. rollDieNTimes: Cats traverse syntax" pipe println
  println("Rolled die 20 times: " + rolled4)

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
