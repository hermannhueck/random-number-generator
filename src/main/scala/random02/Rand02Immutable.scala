package random02

/*
  Immutable generation of pseudo-random values using my own implementation

  The seed is the state of random value generation.
  Each method takes a 'seed: Long' parameter and returns a '(Long, A)'
  where the 1st tuple component is the new seed and the snd is the generated value of the respective type.

  What is a bit cumbersome: We have to thread the seed from one invocation to the next.
 */
object Rand02Immutable extends App {

  println("\n----- Immutable generation of pseudo-random values")

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
    val (seed2, i2) = randomInt(s0)
    (seed2, (i1, i2))
  }

  val s0       = 42
  val (s1, i)  = randomInt(s0)
  val (s2, d)  = randomDouble(s1)
  val (s3, b)  = randomBoolean(s2)
  val (s4, ip) = randomIntPair(s3)

  println("random Int:     " + i)
  println("random Double:  " + d)
  println("random Boolean: " + b)
  println("random IntPair: " + ip)

  println("----- Rolling dies ...")

  def rollDie(seed: Long): (Long, Int) = {
    val (newSeed, i) = nonNegativeInt(seed)
    (newSeed, 1 + i % 6)
  }

  def rollDieNTimes(n: Int)(seed: Long): (Long, List[Int]) = {
    if (n <= 0)
      (seed, List.empty[Int])
    else {
      val (s1, x)  = rollDie(seed)
      val (s2, xs) = rollDieNTimes(n - 1)(s1)
      (s2, x :: xs)
    }
  }

  val (s5, rolled) = rollDieNTimes(20)(s4)
  println("Rolled die 20 times: " + rolled)

  println("-----\n")
}
