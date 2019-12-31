package random02

import scala.util.chaining._
import util._

/*
  Immutable generation of pseudo-random values using my own implementation

  The seed is the state of random value generation.
  Each method takes a 'seed: Long' parameter and returns a '(Long, A)'
  where the 1st tuple component is the new seed and the snd is the generated value of the respective type.

  What is a bit cumbersome: We have to thread the seed from one invocation to the next.
 */
object RandImmutable extends App {

  printStartLine()

  s"----- Immutable generation of pseudo-random values" pipe println

  import Random._

  val s0       = 42
  val (s1, i)  = randomInt(s0)
  val (s2, d)  = randomDouble(s1)
  val (s3, b)  = randomBoolean(s2)
  val (s4, ip) = randomIntPair(s3)

  s"random Int:     $i" pipe println
  s"random Double:  $d" pipe println
  s"random Boolean: $b" pipe println
  s"random IntPair: $ip" pipe println

  "----- Rolling dice ..." pipe println

  def rollDie(seed: Long): (Long, Int) = {
    val (newSeed, i) = nonNegativeInt(seed)
    (newSeed, 1 + i % 6)
  }

  def rollDieNTimes(n: Int)(seed: Long): (Long, List[Int]) = {
    if (n <= 0)
      (seed, List.empty[Int])
    else {
      val (s1, x)  = rollDie(seed)
      val (s2, xs) = rollDieNTimes(n - 1)(s1)
      (s2, x :: xs)
    }
  }

  val (s5, rolled) = rollDieNTimes(20)(s4)
  s"Rolled die 20 times: $rolled" pipe println

  printEndLine()
}
