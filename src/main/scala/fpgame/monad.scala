package fpgame

trait Monad[M[_]] {
  def unit[A](a: A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))
}

object Monad {
  implicit val IOMonad = new Monad[IO] {
    override def unit[A](a: A): IO[A] = IO { a }

    override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma flatMap f
  }
}

case class OptionT[M[_], A] {
  def flatMap[B](f: A => OptionT[M, B])(implicit M: Monad[M]): OptionT[M, B] = ???

  def

}