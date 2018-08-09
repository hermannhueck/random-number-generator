package random

/*
  Immutable generation of pseudo-random values using my own implementation

  The seed is the state of random value generation.
  Each method takes a 'seed: Long' parameter and returns a '(Long, A)'
  where the 1st tuple component is the new seed and the snd is the generated value of the respective type.
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

  def nextLong(seed: Long): (Long, Long) = {
    val newSeed: Long = RNG.nextSeed(seed)
    (newSeed, newSeed) // The return value is a tuple containing both a pseudo-random Long and the next `RNG` state.
  }

  def nextInt(seed: Long): (Long, Int) = {
    val (newSeed, long) = nextLong(seed)
    (newSeed, (long >>> 16).toInt) // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
  } // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  def nonNegativeInt(seed: Long): (Long, Int) = {
    val (newSeed, i) = nextInt(seed)
    val nonNeg = if (i < 0) -(i + 1) else i
    (newSeed, nonNeg)
  }

  def nextDouble(seed: Long): (Long, Double) = {
    val (newSeed, i) = nonNegativeInt(seed)
    val d = i / (Int.MaxValue.toDouble + 1)
    (newSeed, d)
  }

  def nextBoolean(seed: Long): (Long, Boolean) =
    nextInt(seed) match {
      case (newSeed, i) => (newSeed, i % 2 == 0)
    }

  def nextIntPair(seed: Long): (Long, (Int, Int)) = {
    val (seed1, i1) = nextInt(seed)
    val (seed2, i2) = nextInt(s0)
    (seed2, (i1, i2))
  }


  val s0 = 42
  val (s1, int) = nextInt(s0)
  val (s2, double) = nextDouble(s1)
  val (s3, boolean) = nextBoolean(s2)
  val (s4, intPair) = nextIntPair(s3)

  println("random Int:     " + int)
  println("random Double:  " + double)
  println("random Boolean: " + boolean)
  println("random IntPair: " + intPair)


  def rollDie(seed: Long): (Long, Int) = {
    val (newSeed, int) = nonNegativeInt(seed)
    (newSeed, 1 + int % 6)
  }

  def rollDieNTimes(times: Int)(seed: Long): (Long, List[Int]) = {
    if (times <= 0)
      (seed, List.empty[Int])
    else {
      val (s1, x) = rollDie(seed)
      val (s2, xs) = rollDieNTimes(times-1)(s1)
      (s2, x :: xs)
    }
  }


  val (s5, rolled) = rollDieNTimes(20)(s4)
  println(  "Rolled die 20 times: " + rolled  )

  println("-----\n")
}
