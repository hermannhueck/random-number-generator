package random

/*
  Methods to Functions:
  Every random value generation method is refactored to a function of type: RNG => (RNG, Long)
  Most def's can be turned into val's.
 */
object Rand04Functions extends App {

  println("\n----- Moving from methods to functions 'RNG => (RNG, A)'")

  val randomLong: RNG => (RNG, Long) = rng => rng.nextLong

  val randomInt: RNG => (RNG, Int) = rng => {
    val (r, long) = randomLong(rng)
    (r, (long >>> 16).toInt)
  }

  val nonNegativeInt: RNG => (RNG, Int) = rng => {
    val (newRng, i) = randomInt(rng)
    val nonNeg = if (i < 0) -(i + 1) else i
    (newRng, nonNeg)
  }

  val randomDouble: RNG => (RNG, Double) = rng => {
    val (newRng, i) = nonNegativeInt(rng)
    val d = i / (Int.MaxValue.toDouble + 1)
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


  val rng0 = RNG(42)
  val (rng1, i) = randomInt(rng0)
  val (rng2, d) = randomDouble(rng1)
  val (rng3, b) = randomBoolean(rng2)
  val (rng4, ip) = randomIntPair(rng3)

  println("random Int:     " + i)
  println("random Double:  " + d)
  println("random Boolean: " + b)
  println("random IntPair: " + ip)


  println("----- Rolling dies ...")

  val rollDie: RNG => (RNG, Int) = rng => {
    val (newRng, i) = nonNegativeInt(rng)
    (newRng, 1 + i % 6)
  }

  def rollDieNTimes(n: Int): RNG => (RNG, List[Int]) = rng => {
    if (n <= 0)
      (rng, List.empty[Int])
    else {
      val (r1, x) = rollDie(rng)
      val (r2, xs) = rollDieNTimes(n-1)(r1)
      (r2, x :: xs)
    }
  }


  val (rng5, rolled) = rollDieNTimes(20)(rng4)
  println(  "Rolled die 20 times: " + rolled  )

  println("-----\n")
}
