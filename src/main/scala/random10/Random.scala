package random10

import cats.Monad

import libRNG.RNG

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

object State {

  implicit def stateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {

    override def pure[A](x: A): State[S, A] = State { s =>
      (s, x)
    }

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f

    override def tailRecM[A, B](a: A)(f: A => State[S, Either[A, B]]): State[S, B] = ???
  }
}

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

  val intPair: Random[(Int, Int)] =
    for {
      i1 <- int
      i2 <- int
    } yield (i1, i2)
}
