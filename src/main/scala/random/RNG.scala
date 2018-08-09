package random

case class RNG(seed: Long) {

  // Linear congruential generator:
  // https://en.wikipedia.org/wiki/Linear_congruential_generator
  //
  private def nextRNG: RNG = {
    val newSeed: Long =
      (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    RNG(newSeed)
  }

  def nextLong: (RNG, Long) = {
    val rng: RNG = nextRNG
    (rng, rng.seed)
  }
}
