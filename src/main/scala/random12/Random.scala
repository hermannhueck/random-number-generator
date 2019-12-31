package random12

import cats.data.State

import libRNG.RNG
import cats.Applicative

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

  val intPair0: Random[(Int, Int)] =
    for {
      i1 <- int
      i2 <- int
    } yield (i1, i2)

  val intPair: Random[(Int, Int)] =
    tuple2(int, int)

  // map2 with flatMap
  def map2_0[A, B, C](ra: Random[A], rb: Random[B])(f: (A, B) => C): Random[C] =
    for {
      a <- ra
      b <- rb
    } yield f(a, b)

  // map2 without flatMap, manually implemented
  def map2_1[A, B, C](ra: Random[A], rb: Random[B])(f: (A, B) => C): Random[C] = Random { rng =>
    val rng2    = rng.nextLong._1
    val (r1, a) = ra.run(rng).value
    val (r2, b) = rb.run(rng2).value
    (r2, f(a, b))
  }

  // map2 without flatMap, using Applicative
  def map2[A, B, C](ra: Random[A], rb: Random[B])(f: (A, B) => C): Random[C] =
    Applicative[Random].map2(ra, rb)(f)

  // map3 without flatMap, using Applicative
  def map3[A, B, C, D](ra: Random[A], rb: Random[B], rc: Random[C])(f: (A, B, C) => D): Random[D] =
    Applicative[Random].map3(ra, rb, rc)(f)

  // map4 without flatMap, using Applicative
  def map4[A, B, C, D, E](ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D])(f: (A, B, C, D) => E): Random[E] =
    Applicative[Random].map4(ra, rb, rc, rd)(f)

  def tuple2_0[A, B](ra: Random[A], rb: Random[B]): Random[(A, B)] =
    Applicative[Random].tuple2(ra, rb)

  def tuple2[A, B](ra: Random[A], rb: Random[B]): Random[(A, B)] =
    map2(ra, rb)((_, _))

  def tuple3[A, B, C](ra: Random[A], rb: Random[B], rc: Random[C]): Random[(A, B, C)] =
    map3(ra, rb, rc)((_, _, _))

  def tuple4[A, B, C, D](ra: Random[A], rb: Random[B], rc: Random[C], rd: Random[D]): Random[(A, B, C, D)] =
    map4(ra, rb, rc, rd)((_, _, _, _))
}
