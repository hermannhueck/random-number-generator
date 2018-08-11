package random

/*
  This impl step wraps seed generation in 'case class RNG(seed: Long)
  This gives seed generation its own type and makes the client code more robust.

  Still annoying: We have to thread the RNG from one invocation to the next.
 */
object Rand03CaseClassRNG extends App {

  println("\n----- Wrapping seed generation in case class 'RNG'")

  def randomLong(rng: RNG): (RNG, Long) = rng.nextLong

  def randomInt(rng: RNG): (RNG, Int) = {
    val (r, long) = randomLong(rng)
    (r, (long >>> 16).toInt)
  }

  def nonNegativeInt(rng: RNG): (RNG, Int) = {
    val (newRng, i) = randomInt(rng)
    val nonNeg = if (i < 0) -(i + 1) else i
    (newRng, nonNeg)
  }

  def randomDouble(rng: RNG): (RNG, Double) = {
    val (newRng, i) = nonNegativeInt(rng)
    val d = i / (Int.MaxValue.toDouble + 1)
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

  def rollDie(rng: RNG): (RNG, Int) = {
    val (newRng, i) = nonNegativeInt(rng)
    (newRng, 1 + i % 6)
  }

  def rollDieNTimes(n: Int)(rng: RNG): (RNG, List[Int]) = {
    if (n <= 0)
      (rng, List.empty[Int])
    else {
      val (r1, x) = rollDie(rng)
      val (r2, xs) = rollDieNTimes(n-1)(r1)
      (r2, x :: xs)
    }
  }


  val (rng5, rolled) = rollDieNTimes(20)(rng4)
  println("Rolled die 20 times: " + rolled)

  println("-----\n")
}
