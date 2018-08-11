package random

/*
  Functions wrapped in a case class:
  The functions 'RNG => (RNG, Long)' are now wrapped in to a 'case class Random[A](run: RNG => (RNG, Long))'.

  This wrapping gives us a computational context which enables more functional abstractions
  as we will see in the following steps.

  The random value generation functions now return 'Random[A]' and I moved them to the 'Random' companion object.
 */
object Rand05CaseClassRandom extends App {

  println("\n----- Wrapping the functions 'RNG => (RNG, A)' in case class 'Random'")

  final case class Random[A](run: RNG => (RNG, A)) {

  }

  object Random {

    val long: Random[Long] = Random { rng => rng.nextLong }

    val int: Random[Int] = Random { rng =>
      val (r, l) = long.run(rng)
      (r, (l >>> 16).toInt)
    }

    val nonNegativeInt: Random[Int] = Random { rng =>
      val (newRng, i) = int.run(rng)
      val nonNeg = if (i < 0) -(i + 1) else i
      (newRng, nonNeg)
    }

    val double: Random[Double] = Random { rng =>
      val (newRng, i) = nonNegativeInt.run(rng)
      val d = i / (Int.MaxValue.toDouble + 1)
      (newRng, d)
    }

    val boolean: Random[Boolean] = Random { rng =>
      val (newRng, i) = int.run(rng)
      (newRng, i % 2 == 0)
    }

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

  val rollDie: Random[Int] = Random { rng =>
    val (newRng, i) = nonNegativeInt.run(rng)
    (newRng, 1 + i % 6)
  }

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
