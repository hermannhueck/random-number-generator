package random01

object Random {

  type RNG = scala.util.Random

  val rng: RNG = new scala.util.Random(42)

  def randomInt(rng: RNG): Int =
    rng.nextInt

  def randomDouble(rng: RNG): Double =
    rng.nextDouble

  def randomBoolean(rng: RNG): Boolean =
    rng.nextBoolean

  def randomString(length: Int, rng: RNG): String =
    rng.nextString(length)

  def randomIntPair(rng: RNG): (Int, Int) =
    (randomInt(rng), randomInt(rng))
}
