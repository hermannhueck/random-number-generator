package random13

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

  val nonNegativeInt: Random[Int] =
    int map (i => if (i < 0) -(i + 1) else i)

  val double: Random[Double] =
    nonNegativeInt map (i => i / (Int.MaxValue.toDouble + 1))

  val boolean: Random[Boolean] =
    int map (i => i % 2 == 0)

  import cats.syntax.apply._

  val intPair0: Random[(Int, Int)] =
    (int, int) mapN ((_, _))

  val intPair: Random[(Int, Int)] =
    (int, int).tupled
}
