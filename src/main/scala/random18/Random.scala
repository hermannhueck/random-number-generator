package random18

import cats.data.State

import libRNG.RNG

object Random {

  type Random[A] = State[RNG, A]

  def apply[S, A](run: S => (S, A)) =
    State.apply(run)

  val long: Random[Long] =
    apply(rng => rng.nextLong)

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
      int map { i =>
        Math.abs(i % diff) + low
      }
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

  import cats.syntax.apply._

  val intPair: Random[(Int, Int)] =
    (int, int).tupled

  val intDoublePair: Random[(Int, Double)] =
    (int, double).tupled

  val doubleIntPair: Random[(Double, Int)] =
    (double, int).tupled

  val doublePair: Random[(Double, Double)] =
    (double, double).tupled

  val intTriple: Random[(Int, Int, Int)] =
    (int, int, int).tupled

  import cats.instances.list._
  import cats.syntax.traverse._

  def ints(n: Int): Random[List[Int]] =
    (0 until (0 max n)).toList traverse (_ => int)

  def doubles(n: Int): Random[List[Double]] =
    (0 until (0 max n)).toList traverse (_ => double)
}
