package random

import cats.Monad

import scala.language.higherKinds

/*
  The 'Random' Monad:                       case class Random[A](run: RNG => (RNG, A))
  is a special impl of the State Monad:     case class State[S, A](run: S => (S, A))

  In this implementation step we replace 'Random' by the more general State Monad
  and define a type alias which maps Random to State:     type Random[A] = State[RNG, A]
 */
object Rand09AbstractingState extends App {

  println("\n----- Implementing and using a State Monad")

  final case class State[S, A](run: S => (S, A)) {

    def runS: S => S = run(_)._1

    def runA: S => A = run(_)._2

    // map doesn't manipulate the state, it just transforms the A value
    def map[B](f: A => B): State[S, B] = State { s =>
      val (s1, a1) = run(s)
      (s1, f(a1))
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (s1, a1) = run(s)
      f(a1).run(s1)
    }
  }

  implicit def stateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {

    override def pure[A](x: A): State[S, A] = State { s => (s, x) }

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f

    override def tailRecM[A, B](a: A)(f: A => State[S, Either[A, B]]): State[S, B] = ???
  }


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
      for {
        i1 <- nextInt
        i2 <- nextInt
      } yield (i1, i2)
  }


  import Random._

  val rand: Random[(Int, Double, Boolean, (Int, Int))] = for { // program description: doesn't do anything!
    int <- nextInt
    double <- nextDouble
    boolean <- nextBoolean
    intPair <- nextIntPair
  } yield (int, double, boolean, intPair)

  val (newRng, (int, double, boolean, intPair)) = rand.run(RNG(42)) // program invocation

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
      State { rng => (rng, List.empty[Int]) }
    else
      State { rng =>
        val (r1, x) = rollDie.run(rng)
        val (r2, xs) = rollDieNTimes1(times-1).run(r1)
        (r2, x :: xs)
      }

  def rollDieNTimes2(times: Int): Random[List[Int]] =
    if (times <= 0)
      State { rng => (rng, List.empty[Int]) }
    else for {
      x <- rollDie
      xs <- rollDieNTimes2(times-1)
    } yield x :: xs


  val rolled: Random[(List[Int], List[Int])] = for { // program description: doesn't do anything!
    rolled1 <- rollDieNTimes1(20)
    rolled2 <- rollDieNTimes2(20)
  } yield (rolled1, rolled2)

  val (rolled1, rolled2) = rolled.runA(newRng) // program invocation

  println("1. rollDieNTimes: recursive solution")
  println("Rolled die 20 times: " + rolled1)
  println("2. rollDieNTimes: for-comprehension")
  println("Rolled die 20 times: " + rolled2)


  println("-----\n")
}
