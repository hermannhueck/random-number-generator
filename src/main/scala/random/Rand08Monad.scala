package random

import cats.Monad
import libRandom.RNG


/*
  With 'map' and 'flatMap' it is easy to give 'Random' a Monad instance.
  For the Monad instance we must implement 'pure', 'flatMap' and 'tailRecM'
  (As 'tailRecM' is not invoked in this simple example the impl with ??? is sufficient.)
  Now we can use our 'Random' Monad wherever a Monad is required: see 'sumOfSquares'
 */
object Rand08Monad extends App {

  println("\n----- Giving Random a Monad instance")

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

    val intPair: Random[(Int, Int)] =
      for {
        i1 <- int
        i2 <- int
      } yield (i1, i2)

    object instances {

      implicit val randMonad: Monad[Random] = new Monad[Random] {

        override def pure[A](a: A): Random[A] = Random { rng => (rng, a) }

        override def flatMap[A, B](fa: Random[A])(f: A => Random[B]): Random[B] = fa flatMap f

        override def tailRecM[A, B](a: A)(f: A => Random[Either[A, B]]): Random[B] = ???
      }
    }
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


  println("----- Monadic Random ...")

  val rollDie: Random[Int] =
    nonNegativeInt map (i => 1 + i % 6)

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def sumOfSquares[F[_]: Monad](mi1: F[Int], mi2: F[Int]): F[Int] = for {
    i1 <- mi1
    i2 <- mi2
  } yield i1 * i1 + i2 * i2

  import cats.instances.option._

  private val optionResult: Option[Int] = sumOfSquares(Option(3), Option(4))
  println(s"sumOfSquares[Option]: $optionResult")

  import Random.instances._

  private val random = sumOfSquares(rollDie, rollDie)
  val randomResult = random.runA(RNG(42))
  println(s"sumOfSquares[Random]: $randomResult")


  println("----- Rolling dies ...")

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
