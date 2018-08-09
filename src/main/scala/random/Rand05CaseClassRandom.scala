package random

object Rand05CaseClassRandom extends App {

  println("\n----- Wrapping the functions 'RNG => (RNG, A)' in case class 'Random'")

  final case class Random[A](run: RNG => (RNG, A))

  object Random {

    val nextLong: Random[Long] = Random {
      rng => rng.nextLong
    }

    val nextInt: Random[Int] = Random {
      rng => {
        val (r, long) = rng.nextLong
        (r, (long >>> 16).toInt)
      }
    }

    val nonNegativeInt: Random[Int] = Random {
      rng => {
        val (newRng, i) = nextInt.run(rng)
        val nonNeg = if (i < 0) -(i + 1) else i
        (newRng, nonNeg)
      }
    }

    val nextDouble: Random[Double] = Random {
      rng => {
        val (newRng, i) = nonNegativeInt.run(rng)
        val d = i / (Int.MaxValue.toDouble + 1)
        (newRng, d)
      }
    }

    val nextBoolean: Random[Boolean] = Random {
      rng => {
        val (newRng, i) = nextInt.run(rng)
        (newRng, i % 2 == 0)
      }
    }

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


  val rollDie: Random[Int] = Random {
    rng => {
      val (newRng, int) = nonNegativeInt.run(rng)
      (newRng, 1 + int % 6)
    }
  }

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
