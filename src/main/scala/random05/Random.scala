package random05

import libRNG.RNG

final case class Random[A](run: RNG => (RNG, A))

object Random {

  val long: Random[Long] =
    apply(rng => rng.nextLong)

  val int: Random[Int] = Random { rng =>
    val (r, l) = long.run(rng)
    (r, (l >>> 16).toInt)
  }

  val nonNegativeInt: Random[Int] = Random { rng =>
    val (newRng, i) = int.run(rng)
    val nonNeg      = if (i < 0) -(i + 1) else i
    (newRng, nonNeg)
  }

  val double: Random[Double] = Random { rng =>
    val (newRng, i) = nonNegativeInt.run(rng)
    val d           = i / (Int.MaxValue.toDouble + 1)
    (newRng, d)
  }

  val boolean: Random[Boolean] = Random { rng =>
    val (newRng, i) = int.run(rng)
    (newRng, i % 2 == 0)
  }

  val intPair: Random[(Int, Int)] = Random { rng =>
    val (rng1, i1) = int.run(rng)
    val (rng2, i2) = int.run(rng1)
    (rng2, (i1, i2))
  }
}
