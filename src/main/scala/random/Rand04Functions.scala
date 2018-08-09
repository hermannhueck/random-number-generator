package random

/*
  Methods to Functions:
  Every random value generation method is refactored to a function of type: RNG => (RNG, Long)
  Most def's can be turned into val's.
 */
object Rand04Functions extends App {

  println("\n----- Moving from methods to functions 'RNG => (RNG, A)'")

  val nextLong: RNG => (RNG, Long) =
    rng => rng.nextLong

  val nextInt: RNG => (RNG, Int) =
    rng => {
      val (r, long) = rng.nextLong
      (r, (long >>> 16).toInt)
    }

  val nonNegativeInt: RNG => (RNG, Int) =
    rng => {
      val (newRng, i) = nextInt(rng)
      val nonNeg = if (i < 0) -(i + 1) else i
      (newRng, nonNeg)
    }

  val nextDouble: RNG => (RNG, Double) =
    rng => {
      val (newRng, i) = nonNegativeInt(rng)
      val d = i / (Int.MaxValue.toDouble + 1)
      (newRng, d)
    }

  val nextBoolean: RNG => (RNG, Boolean) =
    rng => {
      val (newRng, i) = nextInt(rng)
      (newRng, i % 2 == 0)
    }

  val nextIntPair: RNG => (RNG, (Int, Int)) =
    rng => {
      val (rng1, i1) = nextInt(rng)
      val (rng2, i2) = nextInt(rng1)
      (rng2, (i1, i2))
    }


  val rng0 = RNG(42)
  val (rng1, int) = nextInt(rng0)
  val (rng2, double) = nextDouble(rng1)
  val (rng3, boolean) = nextBoolean(rng2)
  val (rng4, intPair) = nextIntPair(rng3)

  println("random Int:     " + int)
  println("random Double:  " + double)
  println("random Boolean: " + boolean)
  println("random IntPair: " + intPair)


  val rollDie: RNG => (RNG, Int) =
    rng => {
      val (newRng, int) = nonNegativeInt(rng)
      (newRng, 1 + int % 6)
    }

  def rollDieNTimes(times: Int): RNG => (RNG, List[Int]) =
    rng => {
      if (times <= 0)
        (rng, List.empty[Int])
      else {
        val (r1, x) = rollDie(rng)
        val (r2, xs) = rollDieNTimes(times-1)(r1)
        (r2, x :: xs)
      }
    }


  val (rng5, rolled) = rollDieNTimes(20)(rng4)
  println(  "Rolled die 20 times: " + rolled  )

  println("-----\n")
}
