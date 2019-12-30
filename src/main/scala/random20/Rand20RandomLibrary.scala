package random20

import cats.Monad
import libRandom.RNG

/*
  In this step we moved the Random object into its own package 'libRandom'
 */
object Rand20RandomLibrary extends App {

  println("\n----- String Combinators")

  import libRandom.Random._

  val rand = for { // program description: doesn't do anything!
    i        <- int
    d        <- double
    b        <- boolean
    pi       <- pair(int, int)
    ti       <- triple(int, int, int)
    li       <- ints(10)
    ld       <- doubles(10)
    onei     <- oneOf((0 until 9) map (_ * 10): _*)
    ni       <- nOf(20)(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
    cs       <- chars(20)
    s        <- string(20)
    ansi     <- ansiString(20)
    ascii    <- asciiString(20)
    alphaNum <- alphaNumString(80)
    alpha    <- alphaString(20)
    num      <- numericString(20)
  } yield (i, d, b, pi, ti, li, ld, onei, ni, cs, s, ansi, ascii, alphaNum, alpha, num)

  val (newRng, (i, d, b, pi, ti, li, ld, onei, ni, cs, s, ansi, ascii, alphaNum, alpha, num)) = rand.run(RNG(42)).value // program invocation

  println("random Int:          " + i)
  println("random Double:       " + d)
  println("random Boolean:      " + b)
  println("random IntPair:      " + pi)
  println("random IntTriple:    " + ti)
  println("random IntList:      " + li)
  println("random DoubleList:   " + ld)
  println("random oneOf(...):   " + onei)
  println("random nOf(20)(...): " + ni)
  println("random chars:        " + cs)
  println("random String:       " + s)
  println("random ANSI String:  " + ansi)
  println("random ASCII String: " + ascii)
  println("random alphaNum String: " + alphaNum)
  println("random alpha String: " + alpha)
  println("random numeric String: " + num)

  println("----- Monadic Random ...")

  val rollDie: Random[Int] =
    intFromTo(1, 6)

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def sumOfSquares[F[_]: Monad](mi1: F[Int], mi2: F[Int]): F[Int] =
    for {
      i1 <- mi1
      i2 <- mi2
    } yield i1 * i1 + i2 * i2

  import cats.instances.option._

  private val optionResult: Option[Int] = sumOfSquares(Option(3), Option(4))
  println(s"sumOfSquares[Option]: $optionResult")

  private val random = sumOfSquares(rollDie, rollDie)
  val randomResult   = random.runA(RNG(42)).value
  println(s"sumOfSquares[Random]: $randomResult")

  println("----- Rolling dies ...")

  def rollDieNTimes(n: Int): Random[List[Int]] =
    listOf(n)(rollDie)

  val rolled = rollDieNTimes(20).runA(newRng).value
  println("Rolled die 20 times: " + rolled)

  println("-----\n")
}
