package random

object Rand07FlatMap extends App {

  println("\n----- Introducing 'flatMap'")

  final case class Random[A](run: RNG => (RNG, A)) {

    def runA: RNG => A = rng => run(rng)._2

    def map[B](f: A => B): Random[B] = Random {
      rng => {
        val (newRng, a) = run(rng)
        (newRng, f(a))
      }
    }

    def flatMap[B](f: A => Random[B]): Random[B] = Random {
      rng => {
        val (newRng, a) = run(rng)
        f(a).run(newRng)
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

    val nextIntPair0: Random[(Int, Int)] =
      nextInt flatMap {i1 =>
        nextInt map {i2 =>
          (i1, i2)
        }
      }

    val nextIntPair: Random[(Int, Int)] =
      for {
        i1 <- nextInt
        i2 <- nextInt
      } yield (i1, i2)
  }


  import Random._

  val rand: Random[(Int, Double, Boolean, (Int, Int))] = for { // program description: doesn't do anything!
    int <- nextInt
    double <- nextDouble
    boolean <- nextBoolean
    intPair <- nextIntPair
  } yield (int, double, boolean, intPair)

  val (newRng, (int, double, boolean, intPair)) = rand.run(RNG(42)) // program invocation

  println("random Int:     " + int)
  println("random Double:  " + double)
  println("random Boolean: " + boolean)
  println("random IntPair: " + intPair)


  println("----- Rolling dies ...")

  val rollDie: Random[Int] =
    nonNegativeInt map (i => 1 + i % 6)

  def rollDieNTimes1(times: Int): Random[List[Int]] =
    if (times <= 0)
      Random { rng => (rng, List.empty[Int]) }
    else
      Random { rng => {
        val (r1, x) = rollDie.run(rng)
        val (r2, xs) = rollDieNTimes1(times-1).run(r1)
        (r2, x :: xs)
      }}

  def rollDieNTimes2(times: Int): Random[List[Int]] =
    if (times <= 0)
      Random { rng => (rng, List.empty[Int]) }
    else for {
      x <- rollDie
      xs <- rollDieNTimes2(times-1)
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
