package random

/*
  This impl step wraps seed generation in 'case class RNG(seed: Long)
  This gives seed generation its own type and makes the client code more robust.
 */
object Rand03CaseClassRNG extends App {

  println("\n----- Wrapping seed generation in case class 'RNG'")

  def nextLong(rng: RNG): (RNG, Long) =
    rng.nextLong

  def nextInt(rng: RNG): (RNG, Int) = {
    val (r, long) = rng.nextLong
    (r, (long >>> 16).toInt)
  }

  def nonNegativeInt(rng: RNG): (RNG, Int) = {
    val (newRng, i) = nextInt(rng)
    val nonNeg = if (i < 0) -(i + 1) else i
    (newRng, nonNeg)
  }

  def nextDouble(rng: RNG): (RNG, Double) = {
    val (newRng, i) = nonNegativeInt(rng)
    val d = i / (Int.MaxValue.toDouble + 1)
    (newRng, d)
  }

  def nextBoolean(rng: RNG): (RNG, Boolean) =
    nextInt(rng) match {
      case (newRng, i) => (newRng, i % 2 == 0)
    }

  def nextIntPair(rng: RNG): (RNG, (Int, Int)) = {
    val (rng1, i1) = nextInt(rng)
    val (rng2, i2) = nextInt(rng0)
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


  def rollDie(rng: RNG): (RNG, Int) = {
    val (newRng, int) = nonNegativeInt(rng)
    (newRng, 1 + int % 6)
  }

  def rollDieNTimes(times: Int)(rng: RNG): (RNG, List[Int]) = {
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
