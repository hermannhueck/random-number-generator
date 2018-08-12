package random

import cats.Monad
import cats.data.State
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.traverse._
import libRandom.RNG

import scala.language.higherKinds

/*
  This step adds random strings of various kinds.
 */
object Rand19StringCombinators extends App {

  println("\n----- String Combinators")

  type Random[A] = State[RNG, A]

  object Random {

    val long: Random[Long] = State { rng => rng.nextLong }

    val int: Random[Int] =
      long map (l => (l >>> 16).toInt)

    def intFromUntil(lowInclusive: Int, highExclusive: Int): Random[Int] = {

      val (low, high) =
        if (lowInclusive <= highExclusive)
          (lowInclusive, highExclusive)
        else
          (highExclusive, lowInclusive) // flip values if highExclusive < lowInclusive

      val diff = high - low

      if (diff == 0)
        int map (_ => low)
      else {
        // The result is only correct as long as diff <= Int.MaxValue.
        // That is good enough for our demo purposes.
        int map { i => Math.abs(i % diff) + low }
      }
    }

    def intFromTo(lowInclusive: Int, highInclusive: Int): Random[Int] =
      intFromUntil(lowInclusive, highInclusive + 1)

    def intGE(lowInclusive: Int): Random[Int] =
      intFromUntil(lowInclusive, Int.MaxValue)

    def intGT(lowExclusive: Int): Random[Int] =
      intFromUntil(lowExclusive + 1, Int.MaxValue)

    def intLT(highExclusive: Int): Random[Int] =
      intFromUntil(Int.MinValue, highExclusive)

    def intLE(highExclusive: Int): Random[Int] =
      intFromUntil(Int.MinValue, highExclusive + 1)

    def intExactly(value: Int): Random[Int] =
      intFromUntil(value, value)

    val nonNegativeInt: Random[Int] =
      intGE(0)

    val positiveInt: Random[Int] =
      intGT(0)

    val negativeInt: Random[Int] =
      intLT(0)

    val evenInt: Random[Int] =
      int map { i =>
        if (i % 2 == 0) i
        else i + 1
      }

    val oddInt: Random[Int] =
      evenInt map { i =>
        if (i % 2 == 1) i
        else i + 1
      }

    val nonNegativeEvenInt: Random[Int] =
      nonNegativeInt map { i =>
        if (i % 2 == 0) i
        else if (i == Int.MaxValue) 0
        else i + 1
      }

    val nonNegativeOddInt: Random[Int] =
      nonNegativeInt map { i =>
        if (i % 2 == 1) i
        else i + 1
      }

    val double: Random[Double] =
      nonNegativeInt map (i => i / (Int.MaxValue.toDouble + 1))

    val boolean: Random[Boolean] =
      int map (i => i % 2 == 0)

    def pair[A, B](randA: Random[A], randB: Random[B]): Random[(A, B)] =
      (randA, randB).tupled

    def triple[A, B, C](randA: Random[A], randB: Random[B], randC: Random[C]): Random[(A, B, C)] =
      (randA, randB, randC).tupled

    def listOf[A](n: Int)(rand: Random[A]): Random[List[A]] =
      (0 until (0 max n)).toList traverse (_ => rand)

    def ints(n: Int): Random[List[Int]] =
      listOf(n)(int)

    def doubles(n: Int): Random[List[Double]] =
      listOf(n)(double)

    def oneOf[A](seq: A*): Random[A] =
      intFromUntil(0, seq.length) map (index => seq(index))

    def nOf[A](n: Int)(seq: A*): Random[List[A]] =
      listOf(n)(oneOf(seq: _*))

    val char: Random[Char] =
      int map (_.toChar)

    val ansiChar: Random[Char] =
      intFromUntil(0, 256) map (_.toChar)

    val asciiChar: Random[Char] =
      intFromUntil(0, 128) map (_.toChar)

    private val asciiChars: List[Char] = (0 until 127).toList.map(_.toChar)
    private val alphaNumChars: List[Char] = asciiChars.filter(_.isLetterOrDigit)
    private val letters: List[Char] = asciiChars.filter(_.isLetter)
    private val digits: List[Char] = asciiChars.filter(_.isDigit)

    val alphaNumChar: Random[Char] =
      oneOf(alphaNumChars: _*)

    val alphaChar: Random[Char] =
      oneOf(letters: _*)

    val numericChar: Random[Char] =
      oneOf(digits: _*)

    def chars(n: Int): Random[List[Char]] =
      listOf(n)(char)

    def string(n: Int): Random[String] =
      chars(n) map (_.mkString)

    def ansiString(n: Int): Random[String] =
      listOf(n)(ansiChar) map (_.mkString)

    def asciiString(n: Int): Random[String] =
      listOf(n)(asciiChar) map (_.mkString)

    def alphaNumString(n: Int): Random[String] =
      listOf(n)(alphaNumChar) map (_.mkString)

    def alphaString(n: Int): Random[String] =
      listOf(n)(alphaChar) map (_.mkString)

    def numericString(n: Int): Random[String] =
      listOf(n)(numericChar) map (_.mkString)
  }


  import Random._

  val rand = for { // program description: doesn't do anything!
    i <- int
    d <- double
    b <- boolean
    pi <- pair(int, int)
    ti <- triple(int, int, int)
    li <- ints(10)
    ld <- doubles(10)
    onei <- oneOf((0 until 9) map (_ * 10): _*)
    ni <- nOf(20)(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
    cs <- chars(20)
    s <- string(20)
    ansi <- ansiString(20)
    ascii <- asciiString(20)
    alphaNum <- alphaNumString(80)
    alpha <- alphaString(20)
    num <- numericString(20)
  } yield (i, d, b, pi, ti, li, ld, onei, ni, cs, s, ansi, ascii, alphaNum, alpha, num)

  val (newRng, (i, d, b, pi, ti, li, ld, onei, ni, cs, s, ansi, ascii, alphaNum, alpha, num)) = rand.run(RNG(42)).value // program invocation

  println("random Int:          " + i)
  println("random Double:       " + d)
  println("random Boolean:      " + b)
  println("random IntPair:      " + pi)
  println("random IntTriple:    " + ti)
  println("random IntList:      " + li)
  println("random DoubleList:   " + ld)
  println("random oneOf(...):   " + onei)
  println("random nOf(20)(...): " + ni)
  println("random chars:        " + cs)
  println("random String:       " + s)
  println("random ANSI String:  " + ansi)
  println("random ASCII String: " + ascii)
  println("random alphaNum String: " + alphaNum)
  println("random alpha String: " + alpha)
  println("random numeric String: " + num)


  println("----- Monadic Random ...")

  val rollDie: Random[Int] =
    intFromTo(1, 6)

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def sumOfSquares[F[_]: Monad](mi1: F[Int], mi2: F[Int]): F[Int] = for {
    i1 <- mi1
    i2 <- mi2
  } yield i1 * i1 + i2 * i2

  import cats.instances.option._

  private val optionResult: Option[Int] = sumOfSquares(Option(3), Option(4))
  println(s"sumOfSquares[Option]: $optionResult")

  private val random = sumOfSquares(rollDie, rollDie)
  val randomResult = random.runA(RNG(42)).value
  println(s"sumOfSquares[Random]: $randomResult")


  println("----- Rolling dies ...")

  def rollDieNTimes(n: Int): Random[List[Int]] =
    listOf(n)(rollDie)


  val rolled = rollDieNTimes(20).runA(newRng).value
  println("Rolled die 20 times: " + rolled)

  println("-----\n")
}
