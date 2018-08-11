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

    val long: Random[Long] = State { rng => rng.nextLong }

    val int: Random[Int] =
      long map (l => (l >>> 16).toInt)

    val nonNegativeInt: Random[Int] =
      int map (i => if (i < 0) -(i + 1) else i)

    val double: Random[Double] =
      nonNegativeInt map (i => i / (Int.MaxValue.toDouble + 1))

    val boolean: Random[Boolean] =
      int map (i => i % 2 == 0)

    val intPair: Random[(Int, Int)] =
      (int, int).tupled
  }


  import Random._

  val rand: Random[(Int, Double, Boolean, (Int, Int))] = for { // program description: doesn't do anything!
    i <- int
    d <- double
    b <- boolean
    ip <- intPair
  } yield (i, d, b, ip)

  val (newRng, (i, d, b, ip)) = rand.run(RNG(42)).value // program invocation

  println("random Int:     " + i)
  println("random Double:  " + d)
  println("random Boolean: " + b)
  println("random IntPair: " + ip)


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

  def rollDieNTimes1(n: Int): Random[List[Int]] =
    Traverse[List].sequence((0 until (0 max n)).toList map (_ => rollDie))

  def rollDieNTimes2(n: Int): Random[List[Int]] =
    (0 until (0 max n)).toList.map(_ => rollDie).sequence

  def rollDieNTimes3(n: Int): Random[List[Int]] =
    Traverse[List].traverse((0 until (0 max n)).toList)(_ => rollDie)

  def rollDieNTimes4(n: Int): Random[List[Int]] =
    (0 until (0 max n)).toList traverse (_ => rollDie)


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
