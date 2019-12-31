package random12

import cats.Monad
import cats.Applicative

import libRNG.RNG

import scala.util.chaining._
import util._

/*
  'map' uses a function 'A => B' to map a 'Random[A]' to a 'Random[B]'.
  'map2' uses a function '(A, B) => C' to map a 'Random[A]' and a 'Random[B]' to a  a 'Random[C]'.

  Here we provide two implementations of 'map2', one implemented with a for-comprehension
  (i.e. 'map' and 'flatmap') the other one with a manual implementation withour using 'flatMap'.
  Based on 'map2' we can also implement 'tuple2'.

  With 'map2' and 'tuple2' we can easily improve 'nextIntPair' and 'rollDieNTimes2'.
 */
object RandMap2 extends App {

  printStartLine()

  s"----- Implementing and using 'map2'" pipe println

  import Random._

  val rand0: Random[(Int, Double, Boolean, (Int, Int))] = for { // program description: doesn't do anything!
    i  <- int
    d  <- double
    b  <- boolean
    ip <- intPair
  } yield (i, d, b, ip)

  val rand: Random[(Int, Double, Boolean, (Int, Int))] = // program description: doesn't do anything!
    tuple4(int, double, boolean, intPair)

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
      Random(rng => (rng, List.empty[Int]))
    else
      Random { rng =>
        val (r1, x)  = rollDie.run(rng).value
        val (r2, xs) = rollDieNTimes1(n - 1).run(r1).value
        (r2, x :: xs)
      }

  def rollDieNTimes2(n: Int): Random[List[Int]] =
    if (n <= 0)
      Random(rng => (rng, List.empty[Int]))
    else
      map2(rollDie, rollDieNTimes2(n - 1))(_ :: _)

  val rolled0: Random[(List[Int], List[Int])] = for { // program description: doesn't do anything!
    rolled1 <- rollDieNTimes1(20)
    rolled2 <- rollDieNTimes2(20)
  } yield (rolled1, rolled2)

  val rolled: Random[(List[Int], List[Int])] = // program description: doesn't do anything!
    Applicative[Random].tuple2(rollDieNTimes1(20), rollDieNTimes2(20))

  val (rolled1, rolled2) = rolled.runA(newRng).value // program invocation

  "1. rollDieNTimes: recursive solution" pipe println
  s"Rolled die 20 times: $rolled1" pipe println
  "2. rollDieNTimes: solution using map2" pipe println
  s"Rolled die 20 times: $rolled2" pipe println

  "----- Applicative Random ..." pipe println

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def sumOfSquares0[F[_]: Monad](mi1: F[Int], mi2: F[Int]): F[Int] =
    for {
      i1 <- mi1
      i2 <- mi2
    } yield i1 * i1 + i2 * i2

  def sumOfSquares[F[_]: Applicative](mi1: F[Int], mi2: F[Int]): F[Int] =
    Applicative[F].map2(mi1, mi2)((i1, i2) => i1 * i1 + i2 * i2)

  import cats.instances.option._

  private val optionResult: Option[Int] = sumOfSquares(Option(3), Option(4))
  s"sumOfSquares[Option[Int]]: $optionResult" pipe println

  private val random: Random[Int] = sumOfSquares(rollDie, rollDie)
  val randomResult                = random.runA(RNG(42)).value
  s"sumOfSquares[Random[Int]]: $randomResult" pipe println

  printEndLine()
}
