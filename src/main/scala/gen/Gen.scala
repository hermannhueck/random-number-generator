package gen

import cats.data.State
import libRandom.{RNG, Random}

case class Gen[A](sample: State[RNG,A]) {

  def listOfN(n: Int): Gen[List[A]] =
    Gen.listOfN(n)(this)


}

object Gen {

  def pure[A](a: A) =
    Gen(Random.pure(a))

  def unit[A](a: A): Gen[A] =
    pure(a)

  val boolean: Gen[Boolean]
    = Gen(Random.boolean)

  def listOfN[A](n: Int)(gen: Gen[A]): Gen[List[A]] = Gen {
    Random.sequence(List.fill(n)(gen.sample))
  }

  def choose(lowInclusive: Int, highExclusive: Int): Gen[Int] = Gen {
    Random.intFromUntil(lowInclusive, highExclusive)
  }
}
