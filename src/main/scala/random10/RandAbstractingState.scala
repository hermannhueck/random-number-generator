package random10

import cats.Monad
import libRNG.RNG

import scala.util.chaining._
import util._

/*
  The 'Random' Monad:                       case class Random[A](run: RNG => (RNG, A))
  is a special impl of the State Monad:     case class State[S, A](run: S => (S, A))

  In this implementation step we replace 'Random' by the more general State Monad
  and define a type alias which maps Random to State:     type Random[A] = State[RNG, A]
 */
object RandAbstractingState extends App {

  printStartLine()

  s"----- Implementing and using a State Monad" pipe println

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

  "----- Monadic Random ..." pipe println

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def sumOfSquares[F[_]: Monad](mi1: F[Int], mi2: F[Int]): F[Int] =
    for {
      i1 <- mi1
      i2 <- mi2
    } yield i1 * i1 + i2 * i2

  import cats.instances.option._

  private val optionResult: Option[Int] = sumOfSquares(Option(3), Option(4))
  s"sumOfSquares[Option]: $optionResult" pipe println

  private val random = sumOfSquares(rollDie, rollDie)
  val randomResult   = random.runA(RNG(42))
  s"sumOfSquares[Random]: $randomResult" pipe println

  printEndLine()
}
