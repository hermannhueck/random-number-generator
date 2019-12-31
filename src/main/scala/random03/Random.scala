package random03

import libRNG.RNG

object Random {

  def randomLong(rng: RNG): (RNG, Long) =
    rng.nextLong

  def randomInt(rng: RNG): (RNG, Int) = {
    val (r, long) = randomLong(rng)
    (r, (long >>> 16).toInt)
  }

  def nonNegativeInt(rng: RNG): (RNG, Int) = {
    val (newRng, i) = randomInt(rng)
    val nonNeg      = if (i < 0) -(i + 1) else i
    (newRng, nonNeg)
  }

  def randomDouble(rng: RNG): (RNG, Double) = {
    val (newRng, i) = nonNegativeInt(rng)
    val d           = i / (Int.MaxValue.toDouble + 1)
    (newRng, d)
  }

  def randomBoolean(rng: RNG): (RNG, Boolean) = {
    val (newRng, i) = randomInt(rng)
    (newRng, i % 2 == 0)
  }

  def randomIntPair(rng: RNG): (RNG, (Int, Int)) = {
    val (rng1, i1) = randomInt(rng)
    val (rng2, i2) = randomInt(rng1)
    (rng2, (i1, i2))
  }
}
