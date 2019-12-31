package random04

import libRNG.RNG

object Random {

  val randomLong: RNG => (RNG, Long) =
    rng => rng.nextLong

  val randomInt: RNG => (RNG, Int) = rng => {
    val (r, long) = randomLong(rng)
    (r, (long >>> 16).toInt)
  }

  val nonNegativeInt: RNG => (RNG, Int) = rng => {
    val (newRng, i) = randomInt(rng)
    val nonNeg      = if (i < 0) -(i + 1) else i
    (newRng, nonNeg)
  }

  val randomDouble: RNG => (RNG, Double) = rng => {
    val (newRng, i) = nonNegativeInt(rng)
    val d           = i / (Int.MaxValue.toDouble + 1)
    (newRng, d)
  }

  val randomBoolean: RNG => (RNG, Boolean) = rng => {
    val (newRng, i) = randomInt(rng)
    (newRng, i % 2 == 0)
  }

  val randomIntPair: RNG => (RNG, (Int, Int)) = rng => {
    val (rng1, i1) = randomInt(rng)
    val (rng2, i2) = randomInt(rng1)
    (rng2, (i1, i2))
  }
}
