package random

import cats.Monad
import cats.data.State

import scala.language.higherKinds

/*
  'map' uses a function 'A => B' to map a 'Random[A]' to a 'Random[B]'.
  'map2' uses a function '(A, B) => C' to map a 'Random[A]' and a 'Random[B]' to a  a 'Random[C]'.

  Here we provide two implementations of 'map2', one implemented with a for-comprehension
  (i.e. 'map' and 'flatmap') the other one with a manual implementation withour using 'flatMap'.
  Based on 'map2' we can also implement 'tuple2'.

  With 'map2' and 'tuple2' we can easily improve 'nextIntPair' and 'rollDieNTimes2'.
 */
object Rand11Map2 extends App {

  println("\n----- Implementing and using a 'map2'")

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

    val nextIntPair0: Random[(Int, Int)] =
      for {
        i1 <- nextInt
        i2 <- nextInt
      } yield (i1, i2)

    val nextIntPair: Random[(Int, Int)] =
      tuple2(nextInt, nextInt)


    // map2 with flatMap
    def map2_0[A, B, C](ra: Random[A], rb: Random[B])(f: (A, B) => C): Random[C] =
      for {
        a <- ra
        b <- rb
      } yield f(a, b)

    // map2 without flatMap
    def map2[A, B, C](ra: Random[A], rb: Random[B])(f: (A, B) => C): Random[C] = State {
      rng => {
        val (r1, a) = ra.run(rng).value
        val (r2, b) = rb.run(r1).value
        (r2, f(a, b))
      }
    }

    def tuple2[A, B](ra: Random[A], rb: Random[B]): Random[(A, B)] =
      map2(ra, rb)((_, _))
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

  def sumOfSquares[F[_]: Monad](m1: F[Int], m2: F[Int]): F[Int] =
    for {
      i1 <- m1
      i2 <- m2
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
      State { rng => (rng, List.empty[Int]) }
    else
      State { rng =>
        val (r1, x) = rollDie.run(rng).value
        val (r2, xs) = rollDieNTimes1(times-1).run(r1).value
        (r2, x :: xs)
      }

  def rollDieNTimes2(times: Int): Random[List[Int]] =
    if (times <= 0)
      State { rng => (rng, List.empty[Int]) }
    else
      map2(rollDie, rollDieNTimes2(times-1))(_ :: _)


  val rolled: Random[(List[Int], List[Int])] = for { // program description: doesn't do anything!
    rolled1 <- rollDieNTimes1(20)
    rolled2 <- rollDieNTimes2(20)
  } yield (rolled1, rolled2)

  val (rolled1, rolled2) = rolled.runA(newRng).value // program invocation

  println("1. rollDieNTimes: recursive solution")
  println("Rolled die 20 times: " + rolled1)
  println("2. rollDieNTimes: solution with map2")
  println("Rolled die 20 times: " + rolled2)

  println("-----\n")
}
