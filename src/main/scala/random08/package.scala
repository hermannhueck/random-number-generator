package object random08 {

  implicit class MonadSyntax[F[_]: Monad, A](fa: F[A]) {

    def flatMap[B](f: A => F[B]): F[B] =
      Monad[F].flatMap(fa)(f)

    def map[B](f: A => B): F[B] =
      Monad[F].map(fa)(f)
  }
}
