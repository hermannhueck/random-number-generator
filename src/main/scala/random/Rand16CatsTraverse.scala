package random

import cats.data.State
import cats.syntax.apply._
import cats.{Monad, Traverse}

import scala.language.higherKinds

/*
  Using typeclass 'cats.Traverse'

  'cats.Traverse' provides traversing and sequencing which I implemented myself.
  In this step I replace the usage of my own impl
  with the usage of 'cats.Traverse.sequence' and 'cats.Traverse.traverse'.
 */
object Rand16CatsTraverse extends App {

  println("\n----- Using Cats 'Traverse.sequence' and 'Traverse.traverse'")

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

  import cats.instances.list._
  import cats.syntax.traverse._

  def rollDieNTimes1(times: Int): Random[List[Int]] =
    Traverse[List].sequence((0 until (0 max times)).toList map (_ => rollDie))

  def rollDieNTimes2(times: Int): Random[List[Int]] =
    (0 until (0 max times)).toList.map(_ => rollDie).sequence

  def rollDieNTimes3(times: Int): Random[List[Int]] =
    Traverse[List].traverse((0 until (0 max times)).toList)(_ => rollDie)

  def rollDieNTimes4(times: Int): Random[List[Int]] =
    (0 until (0 max times)).toList traverse (_ => rollDie)


  val rolled = for { // program description: doesn't do anything!
    rolled1 <- rollDieNTimes1(20)
    rolled2 <- rollDieNTimes2(20)
    rolled3 <- rollDieNTimes3(20)
    rolled4 <- rollDieNTimes4(20)
  } yield (rolled1, rolled2, rolled3, rolled4)

  val (rolled1, rolled2, rolled3, rolled4) = rolled.runA(newRng).value // program invocation

  println("1. rollDieNTimes: Cats Traverse[List].sequence")
  println("Rolled die 20 times: " + rolled1)
  println("2. rollDieNTimes: Cats sequence syntax")
  println("Rolled die 20 times: " + rolled2)
  println("3. rollDieNTimes: Cats Traverse[List].traverse")
  println("Rolled die 20 times: " + rolled3)
  println("4. rollDieNTimes: Cats traverse syntax")
  println("Rolled die 20 times: " + rolled4)

  println("-----\n")
}
