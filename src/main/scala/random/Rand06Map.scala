package random

/*
  Now we implement 'map' on our 'case class Random'.
  Using 'map' we can simplify many of our random value generation functions.
 */
object Rand06Map extends App {

  println("\n----- Introducing 'map'")

  final case class Random[A](run: RNG => (RNG, A)) {

    def map[B](f: A => B): Random[B] = Random { rng =>
      val (newRng, a) = run(rng)
      (newRng, f(a))
    }
  }

  object Random {

    val long: Random[Long] = Random { rng => rng.nextLong }

    val int: Random[Int] =
      long map (l => (l >>> 16).toInt)

    val nonNegativeInt: Random[Int] =
      int map (i => if (i < 0) -(i + 1) else i)

    val double: Random[Double] =
      nonNegativeInt map (i => i / (Int.MaxValue.toDouble + 1))

    val boolean: Random[Boolean] =
      int map (i => i % 2 == 0)

    val intPair: Random[(Int, Int)] = Random { rng =>
      val (rng1, i1) = int.run(rng)
      val (rng2, i2) = int.run(rng1)
      (rng2, (i1, i2))
    }
  }


  import Random._

  val rng0 = RNG(42)
  val (rng1, i) = int.run(rng0)
  val (rng2, d) = double.run(rng1)
  val (rng3, b) = boolean.run(rng2)
  val (rng4, ip) = intPair.run(rng3)

  println("random Int:     " + i)
  println("random Double:  " + d)
  println("random Boolean: " + b)
  println("random IntPair: " + ip)


  println("----- Rolling dies ...")

  val rollDie: Random[Int] =
    nonNegativeInt map (i => 1 + i % 6)

  def rollDieNTimes(n: Int): Random[List[Int]] = Random { rng =>
    if (n <= 0)
      (rng, List.empty[Int])
    else {
      val (r1, x) = rollDie.run(rng)
      val (r2, xs) = rollDieNTimes(n-1).run(r1)
      (r2, x :: xs)
    }
  }


  val (rng5, rolled) = rollDieNTimes(20).run(rng4)
  println("Rolled die 20 times: " + rolled)

  println("-----\n")
}
