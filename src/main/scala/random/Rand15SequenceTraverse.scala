package random

import cats.{Applicative, Monad}
import cats.data.State
import cats.syntax.apply._

import scala.language.higherKinds

/*
  Deducing 'sequence' and 'traverse'

  We create other implementations of 'rollDieNTimesX':

  rollDieNTimes3: creates a List[Random[Int]] with each element referencing 'rollDie'
      It then traverses that list running the Random for each element thus generating a new value between 1 and 6 inclusive
      finally returning a List of die rolls (i.e. List[Int]) wrapped in a Random.

  rollDieNTimes4: separates sequencing from list creation (mapping a List[Int] to List[Random[Int]].
      It moves the foldRight into its own function 'sequenceRands'.
      'sequenceRands' turns a List[Random[Int]] into a Random[List[Int]] (the sequence pattern).
      This impl need two list traversals, one for the mapping and on for sequencing.

  rollDieNTimes5: integrates mapping and sequencing into one traversal of a List[Int].
      'traverseRands' takes a List[Int] and the mapping function Int => Random[Int] returning Random[List[Int]]

  rollDieNTimes6: uses traverse, the generic version of 'traverseRands'.
      'traverse' takes a List[A] and the mapping function A => Random[B] returning Random[List[B]]

  rollDieNTimes7: uses 'sequence' the generic version of 'sequenceRands' (implemented with 'traverse' and the 'identity' function).
      'sequence' takes a List[Random[A]] and returns a Random[List[B]]
 */
object Rand15SequenceTraverse extends App {

  println("\n----- Deducing 'sequence' and 'traverse'")

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
      Applicative[Random].pure[List[Int]](List.empty[Int])
    else
      State { rng =>
        val (r1, x) = rollDie.run(rng).value
        val (r2, xs) = rollDieNTimes1(times-1).run(r1).value
        (r2, x :: xs)
      }

  import cats.syntax.applicative._

  def rollDieNTimes2(times: Int): Random[List[Int]] =
    if (times <= 0)
      List.empty[Int].pure[Random]
    else
      (rollDie, rollDieNTimes2(times-1)) mapN (_ :: _)

  def rollDieNTimes3(times: Int): Random[List[Int]] = State { rng =>
    val randList: List[Random[Int]] = (0 until (0 max times)).toList map (_ => rollDie)
    randList.foldRight((rng, List.empty[Int])) { (rand, acc) =>
      val (oldRng, xs) = acc
      val (newRng, x) = rand.run(oldRng).value
      (newRng, x :: xs)
    }
  }

  def rollDieNTimes4(times: Int): Random[List[Int]] =
    sequenceRands((0 until (0 max times)).toList map (_ => rollDie))

  def sequenceRands(randList: List[Random[Int]]): Random[List[Int]] = State { rng =>
    randList.foldRight((rng, List.empty[Int])) { (rand, acc) =>
      val (oldRng, xs) = acc
      val (newRng, x) = rand.run(oldRng).value
      (newRng, x :: xs)
    }
  }

  def rollDieNTimes5(times: Int): Random[List[Int]] =
     traverseRands((0 until (0 max times)).toList)(_ => rollDie)

  def traverseRands(intList: List[Int])(f: Int => Random[Int]): Random[List[Int]] = State { rng =>
    intList.foldRight((rng, List.empty[Int])) { (int, acc) =>
      val (oldRng, xs) = acc
      val (newRng, x) = f(int).run(oldRng).value
      (newRng, x :: xs)
    }
  }

  def rollDieNTimes6(times: Int): Random[List[Int]] =
    traverse((0 until (0 max times)).toList)(_ => rollDie)

  def traverse[A, B](list: List[A])(f: A => Random[B]): Random[List[B]] = State { rng =>
    list.foldRight((rng, List.empty[B])) { (a, acc) =>
      val (oldRng, bs) = acc
      val (newRng, b) = f(a).run(oldRng).value
      (newRng, b :: bs)
    }
  }

  def rollDieNTimes7(times: Int): Random[List[Int]] =
    sequence((0 until (0 max times)).toList map (_ => rollDie))

  def sequence[A, B](list: List[Random[A]]): Random[List[A]] =
    traverse(list)(identity)


  val rolled = for { // program description: doesn't do anything!
    rolled1 <- rollDieNTimes1(20)
    rolled2 <- rollDieNTimes2(20)
    rolled3 <- rollDieNTimes3(20)
    rolled4 <- rollDieNTimes4(20)
    rolled5 <- rollDieNTimes5(20)
    rolled6 <- rollDieNTimes6(20)
    rolled7 <- rollDieNTimes7(20)
  } yield (rolled1, rolled2, rolled3, rolled4, rolled5, rolled6, rolled7)

  val (rolled1, rolled2, rolled3, rolled4, rolled5, rolled6, rolled7) = rolled.runA(newRng).value // program invocation

  println("1. rollDieNTimes: recursive solution")
  println("Rolled die 20 times: " + rolled1)
  println("2. rollDieNTimes: solution with mapN")
  println("Rolled die 20 times: " + rolled2)
  println("3. rollDieNTimes: solution with List#foldRight")
  println("Rolled die 20 times: " + rolled3)
  println("4. rollDieNTimes: factored out sequencing")
  println("Rolled die 20 times: " + rolled4)
  println("5. rollDieNTimes: from sequence to traverse")
  println("Rolled die 20 times: " + rolled5)
  println("6. rollDieNTimes: generic traverse")
  println("Rolled die 20 times: " + rolled6)
  println("7. rollDieNTimes: sequence (implemented with traverse)")
  println("Rolled die 20 times: " + rolled7)

  println("-----\n")
}
