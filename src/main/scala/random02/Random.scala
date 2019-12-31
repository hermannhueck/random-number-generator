package random02

object Random {

  object RNG {

    // Linear congruential generator:
    // https://en.wikipedia.org/wiki/Linear_congruential_generator
    //
    def nextSeed(seed: Long): Long =
      (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
  }

  def randomLong(seed: Long): (Long, Long) = {
    val newSeed: Long = RNG.nextSeed(seed)
    (newSeed, newSeed) // The return value is a tuple containing both a pseudo-random Long and the next `RNG` state.
  }

  def randomInt(seed: Long): (Long, Int) = {
    val (newSeed, long) = randomLong(seed)
    (newSeed, (long >>> 16).toInt) // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
  }                                // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  def nonNegativeInt(seed: Long): (Long, Int) = {
    val (newSeed, i) = randomInt(seed)
    val nonNeg       = if (i < 0) -(i + 1) else i
    (newSeed, nonNeg)
  }

  def randomDouble(seed: Long): (Long, Double) = {
    val (newSeed, i) = nonNegativeInt(seed)
    val d            = i / (Int.MaxValue.toDouble + 1)
    (newSeed, d)
  }

  def randomBoolean(seed: Long): (Long, Boolean) = {
    val (newSeed, i) = randomInt(seed)
    (newSeed, i % 2 == 0)
  }

  def randomIntPair(seed: Long): (Long, (Int, Int)) = {
    val (seed1, i1) = randomInt(seed)
    val (seed2, i2) = randomInt(seed1)
    (seed2, (i1, i2))
  }
}
