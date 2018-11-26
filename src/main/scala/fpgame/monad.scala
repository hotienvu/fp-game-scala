package fpgame

import scala.io.StdIn

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

case class OptionT[M[_], A](run: M[Option[A]]) {
  def flatMap[B](f: A => OptionT[M, B])(implicit M: Monad[M]): OptionT[M, B] = OptionT(M.flatMap(run) {
    case None => M.unit(None)
    case Some(a) => f(a).run
  })

  def map[B](f: A => B)(implicit M: Monad[M]): OptionT[M, B] = flatMap(a => OptionT.unit(f(a)))
}

object OptionT {
  def unit[M[_], A](a: A)(implicit M: Monad[M]): OptionT[M, A] = OptionT(M.unit(Some(a)))
}

object OptionTTest {
  def main(args: Array[String]): Unit = {
    def foo(s: String): IO[Option[String]] = IO {
      StdIn.readLine(s"Enter $s:") match {
        case "" => None
        case x => Some(x)
      }
    }

    def bar(a: String): IO[Option[String]] = IO {
      if (a.contains(" ")) Some(a) else None
    }

    val x = for {
      y <- OptionT(foo("test"))
      z <- OptionT(bar(y))
    } yield z
    println(x.run.run)
  }
}