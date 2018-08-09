package random

import cats.Monad
import cats.data.State
import cats.syntax.apply._

import scala.language.higherKinds

/*
  In 'rollDieNTimes1' and 'rollDieNTimes2' we just list an empty List[Int] into a 'Random' context
  in case the specified parameter 'times' is <= 0.

  The 'pure' does exactly this. More general: 'pure' lifts any value A into a computational context
  (here: Random[A] which is aliased to State[RNG, A]) which provides a minimal representation of that value.

  Here we provide 'pure' for 'Random' and use it in 'rollDieNTimes1' and 'rollDieNTimes2'
  where A is List[Int].
 */
object Rand13Pure extends App {

  println("\n----- Implementing and using 'Random.pure'")

  type Random[A] = State[RNG, A]

  object Random {

    val nextLong: Random[Long] = State { rng => rng.nextLong }

    val nextInt: Random[Int] =
      nextLong map (l => (l >>> 16).toInt)

    val nonNegativeInt: Random[Int] =
      nextInt map (i => if (i < 0) -(i + 1) else i)

    val nextDouble: Random[Double] =
      nonNegativeInt map (i => i / (Int.MaxValue.toDouble + 1))

    val nextBoolean: Random[Boolean] =
      nextInt map (i => i % 2 == 0)

    val nextIntPair: Random[(Int, Int)] =
      (nextInt, nextInt).tupled

    def pure[A](a: A): Random[A] = State { rng => (rng, a) }
  }


  import Random._

  val rand: Random[(Int, Double, Boolean, (Int, Int))] = for { // program description: doesn't do anything!
    int <- nextInt
    double <- nextDouble
    boolean <- nextBoolean
    intPair <- nextIntPair
  } yield (int, double, boolean, intPair)

  val (newRng, (int, double, boolean, intPair)) = rand.run(RNG(42)).value // program invocation

  println("random Int:     " + int)
  println("random Double:  " + double)
  println("random Boolean: " + boolean)
  println("random IntPair: " + intPair)


  println("----- Monadic Random ...")

  val rollDie: Random[Int] =
    nonNegativeInt map (i => 1 + i % 6)

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def sumOfSquares[F[_]: Monad](mi1: F[Int], mi2: F[Int]): F[Int] = for {
    i1 <- mi1
    i2 <- mi2
  } yield i1 * i1 + i2 * i2

  import cats.instances.option._

  private val optionResult: Option[Int] = sumOfSquares(Option(3), Option(4))
  println(s"sumOfSquares[Option]: $optionResult")

  private val random = sumOfSquares(rollDie, rollDie)
  val randomResult = random.runA(RNG(42))
  println(s"sumOfSquares[Random]: $randomResult")


  println("----- Rolling dies ...")

  def rollDieNTimes1(times: Int): Random[List[Int]] =
    if (times <= 0)
      pure(List.empty[Int])
    else
      State { rng =>
        val (r1, x) = rollDie.run(rng).value
        val (r2, xs) = rollDieNTimes1(times-1).run(r1).value
        (r2, x :: xs)
      }

  def rollDieNTimes2(times: Int): Random[List[Int]] =
    if (times <= 0)
      pure(List.empty[Int])
    else
      (rollDie, rollDieNTimes2(times-1)) mapN (_ :: _)


  val rolled: Random[(List[Int], List[Int])] = for { // program description: doesn't do anything!
    rolled1 <- rollDieNTimes1(20)
    rolled2 <- rollDieNTimes2(20)
  } yield (rolled1, rolled2)

  val (rolled1, rolled2) = rolled.runA(newRng).value // program invocation

  println("1. rollDieNTimes: recursive solution")
  println("Rolled die 20 times: " + rolled1)
  println("2. rollDieNTimes: solution with mapN")
  println("Rolled die 20 times: " + rolled2)

  println("-----\n")
}
