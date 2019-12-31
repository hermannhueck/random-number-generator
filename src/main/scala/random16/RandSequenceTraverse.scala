package random16

import cats.Applicative

import libRNG.RNG

import scala.util.chaining._
import util._

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
object RandSequenceTraverse extends App {

  printStartLine()

  s"----- Deducing 'sequence' and 'traverse'" pipe println

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

  def rollDieNTimes1(n: Int): Random[List[Int]] =
    if (n <= 0)
      Applicative[Random].pure[List[Int]](List.empty[Int])
    else
      Random { rng =>
        val (r1, x)  = rollDie.run(rng).value
        val (r2, xs) = rollDieNTimes1(n - 1).run(r1).value
        (r2, x :: xs)
      }

  import cats.syntax.applicative._

  def rollDieNTimes2(n: Int): Random[List[Int]] =
    if (n <= 0)
      List.empty[Int].pure[Random]
    else
      (rollDie, rollDieNTimes2(n - 1)) mapN (_ :: _)

  def rollDieNTimes3(n: Int): Random[List[Int]] =
    Random { rng =>
      val randList: List[Random[Int]] = (0 until (0 max n)).toList map (_ => rollDie)
      randList.foldRight((rng, List.empty[Int])) { (rand, acc) =>
        val (oldRng, xs) = acc
        val (newRng, x)  = rand.run(oldRng).value
        (newRng, x :: xs)
      }
    }

  def rollDieNTimes4(n: Int): Random[List[Int]] =
    sequenceRands((0 until (0 max n)).toList map (_ => rollDie))

  private def sequenceRands(randList: List[Random[Int]]): Random[List[Int]] = Random { rng =>
    randList.foldRight((rng, List.empty[Int])) { (rand, acc) =>
      val (oldRng, xs) = acc
      val (newRng, x)  = rand.run(oldRng).value
      (newRng, x :: xs)
    }
  }

  def rollDieNTimes5(n: Int): Random[List[Int]] =
    traverseRands((0 until (0 max n)).toList)(_ => rollDie)

  private def traverseRands(intList: List[Int])(f: Int => Random[Int]): Random[List[Int]] =
    Random { rng =>
      intList.foldRight((rng, List.empty[Int])) { (int, acc) =>
        val (oldRng, xs) = acc
        val (newRng, x)  = f(int).run(oldRng).value
        (newRng, x :: xs)
      }
    }

  def rollDieNTimes6(n: Int): Random[List[Int]] =
    traverse((0 until (0 max n)).toList)(_ => rollDie)

  private def traverse[A, B](list: List[A])(f: A => Random[B]): Random[List[B]] = Random { rng =>
    list.foldRight((rng, List.empty[B])) { (a, acc) =>
      val (oldRng, bs) = acc
      val (newRng, b)  = f(a).run(oldRng).value
      (newRng, b :: bs)
    }
  }

  def rollDieNTimes7(n: Int): Random[List[Int]] =
    sequence((0 until (0 max n)).toList map (_ => rollDie))

  private def sequence[A, B](list: List[Random[A]]): Random[List[A]] =
    traverse(list)(identity)

  val rolled = // program description: doesn't do anything!
    (
      rollDieNTimes1(20),
      rollDieNTimes2(20),
      rollDieNTimes3(20),
      rollDieNTimes4(20),
      rollDieNTimes5(20),
      rollDieNTimes6(20),
      rollDieNTimes7(20)
    ).tupled

  val (rolled1, rolled2, rolled3, rolled4, rolled5, rolled6, rolled7) = rolled.runA(newRng).value // program invocation

  "1. rollDieNTimes: recursive solution" pipe println
  s"Rolled die 20 times: $rolled1" pipe println
  "2. rollDieNTimes: solution using mapN" pipe println
  s"Rolled die 20 times: $rolled2" pipe println
  s"3. rollDieNTimes: solution with List#foldRight" pipe println
  s"Rolled die 20 times: $rolled3" pipe println
  s"4. rollDieNTimes: factored out sequencing" pipe println
  s"Rolled die 20 times: $rolled4" pipe println
  s"5. rollDieNTimes: from sequence to traverse" pipe println
  s"Rolled die 20 times: $rolled5" pipe println
  s"6. rollDieNTimes: generic traverse" pipe println
  s"Rolled die 20 times: $rolled6" pipe println
  s"7. rollDieNTimes: generic sequence (implemented with traverse)" pipe println
  s"Rolled die 20 times: $rolled7" pipe println

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
