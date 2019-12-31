package random08

import libRNG.RNG

final case class Random[A](run: RNG => (RNG, A)) {

  def runA: RNG => A = rng => run(rng)._2

  def map[B](f: A => B): Random[B] = Random { rng =>
    val (newRng, a) = run(rng)
    (newRng, f(a))
  }

  def flatMap[B](f: A => Random[B]): Random[B] = Random { rng =>
    val (newRng, a) = run(rng)
    f(a).run(newRng)
  }
}

object Random {

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

  implicit val randMonad: Monad[Random] = new Monad[Random] {

    override def pure[A](a: A): Random[A] =
      Random(rng => (rng, a))

    override def flatMap[A, B](fa: Random[A])(f: A => Random[B]): Random[B] =
      fa flatMap f
  }
}
