package random

/*
  Implementing 'flatMap' opens up the possibility to improve 'nextIntPair'.
  This also allows us to implement 'rollDieNTimes2' with a for-comprehension.

  With 'map' and 'flatMap' we can mostly avoid threading the RNG through
  the functions or through the program.
 */
object Rand07FlatMap extends App {

  println("\n----- Introducing 'flatMap'")

  final case class Random[A](run: RNG => (RNG, A)) {

    def runA: RNG => A = rng => run(rng)._2

    def map[B](f: A => B): Random[B] = Random { rng =>
      val (newRng, a) = run(rng)
      (newRng, f(a))
    }

    def flatMap[B](f: A => Random[B]): Random[B] = Random { rng =>
      val (newRng, a) = run(rng)
      f(a).run(newRng)
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

    val intPair0: Random[(Int, Int)] =
      int flatMap { i1 =>
        int map { i2 =>
          (i1, i2)
        }
      }

    val intPair: Random[(Int, Int)] =
      for {
        i1 <- int
        i2 <- int
      } yield (i1, i2)
  }


  import Random._

  val rand: Random[(Int, Double, Boolean, (Int, Int))] = for { // program description: doesn't do anything!
    i <- int
    d <- double
    b <- boolean
    ip <- intPair
  } yield (i, d, b, ip)

  val (newRng, (i, d, b, ip)) = rand.run(RNG(42)) // program invocation

  println("random Int:     " + i)
  println("random Double:  " + d)
  println("random Boolean: " + b)
  println("random IntPair: " + ip)


  println("----- Rolling dies ...")

  val rollDie: Random[Int] =
    nonNegativeInt map (i => 1 + i % 6)

  def rollDieNTimes1(n: Int): Random[List[Int]] =
    if (n <= 0)
      Random { rng => (rng, List.empty[Int]) }
    else
      Random { rng =>
        val (r1, x) = rollDie.run(rng)
        val (r2, xs) = rollDieNTimes1(n-1).run(r1)
        (r2, x :: xs)
      }

  def rollDieNTimes2(n: Int): Random[List[Int]] =
    if (n <= 0)
      Random { rng => (rng, List.empty[Int]) }
    else for {
      x <- rollDie
      xs <- rollDieNTimes2(n-1)
    } yield x :: xs


  val rolled: Random[(List[Int], List[Int])] = for { // program description: doesn't do anything!
    rolled1 <- rollDieNTimes1(20)
    rolled2 <- rollDieNTimes2(20)
  } yield (rolled1, rolled2)

  val (rolled1, rolled2) = rolled.runA(newRng) // program invocation

  println("1. rollDieNTimes: recursive solution")
  println("Rolled die 20 times: " + rolled1)
  println("2. rollDieNTimes: for-comprehension")
  println("Rolled die 20 times: " + rolled2)

  println("-----\n")
}
