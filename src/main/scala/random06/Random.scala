package random06

import libRNG.RNG

final case class Random[A](run: RNG => (RNG, A)) {

  def map[B](f: A => B): Random[B] = Random { rng =>
    val (newRng, a) = run(rng)
    (newRng, f(a))
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

  val intPair: Random[(Int, Int)] = Random { rng =>
    val (rng1, i1) = int.run(rng)
    val (rng2, i2) = int.run(rng1)
    (rng2, (i1, i2))
  }
}
