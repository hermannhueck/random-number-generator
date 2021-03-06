package random08

trait Monad[F[_]] {

  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  final def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  final def flatten[A](fa: F[F[A]]): F[A] =
    flatMap(fa)(identity)
}

object Monad {

  // summoner
  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]
  // def apply[F[_]](implicit m: Monad[F]): Monad[F] = m

  implicit val listMonad: Monad[List] = new Monad[List] {

    override def pure[A](a: A): List[A] =
      List(a)

    override def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list flatMap f
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {

    override def pure[A](a: A): Option[A] =
      Option(a)

    override def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] =
      option flatMap f
  }
}
