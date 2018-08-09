package random

/*
  Now we implement 'map' on our 'case class Random'.
  Using 'map' we can simplify many of our random value generation functions.
 */
object Rand06Map extends App {

  println("\n----- Introducing 'map'")

  final case class Random[A](run: RNG => (RNG, A)) {

    def map[B](f: A => B): Random[B] = Random {
      rng => {
        val (newRng, a) = run(rng)
        (newRng, f(a))
      }
    }
  }

  object Random {

    val nextLong: Random[Long] =
      Random { rng => rng.nextLong }

    val nextInt: Random[Int] =
      nextLong map (l => (l >>> 16).toInt)

    val nonNegativeInt: Random[Int] =
      nextInt map (i => if (i < 0) -(i + 1) else i)

    val nextDouble: Random[Double] =
      nonNegativeInt map (i => i / (Int.MaxValue.toDouble + 1))

    val nextBoolean: Random[Boolean] =
      nextInt map (i => i % 2 == 0)

    val nextIntPair: Random[(Int, Int)] = Random {
      rng => {
        val (rng1, i1) = nextInt.run(rng)
        val (rng2, i2) = nextInt.run(rng1)
        (rng2, (i1, i2))
      }
    }
  }


  import Random._

  val rng0 = RNG(42)
  val (rng1, int) = nextInt.run(rng0)
  val (rng2, double) = nextDouble.run(rng1)
  val (rng3, boolean) = nextBoolean.run(rng2)
  val (rng4, intPair) = nextIntPair.run(rng3)

  println("random Int:     " + int)
  println("random Double:  " + double)
  println("random Boolean: " + boolean)
  println("random IntPair: " + intPair)


  val rollDie: Random[Int] =
    nonNegativeInt map (i => 1 + i % 6)

  def rollDieNTimes(times: Int): Random[List[Int]] = Random {
    rng => {
      if (times <= 0)
        (rng, List.empty[Int])
      else {
        val (r1, x) = rollDie.run(rng)
        val (r2, xs) = rollDieNTimes(times-1).run(r1)
        (r2, x :: xs)
      }
    }
  }


  val (rng5, rolled) = rollDieNTimes(20).run(rng4)
  println(  "Rolled die 20 times: " + rolled  )

  println("-----\n")
}
