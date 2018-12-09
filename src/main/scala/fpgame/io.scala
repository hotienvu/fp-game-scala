package fpgame

import scala.io.StdIn

trait IO[A] {
  def run: A

  def flatMap[B](f: A => IO[B]): IO[B] = IO { f(run).run }

  def map[B](f: A => B): IO[B] = flatMap(a => IO { f(a) })
}

object IO extends Monad[IO] {
  override def unit[A](a: A): IO[A] = apply(a)

  override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma flatMap f

  def apply[A](a: => A): IO[A] = new IO[A] { override def run: A = a }

  def getLine(s: String): IO[String] = IO { StdIn.readLine(s) }

  def putStrln(s: String): IO[Unit] = IO { Console.println(s) }

  def putStr(s: String): IO[Unit] = IO { print(s) }

  def loop[A](ma: IO[A]): IO[A] = ma.flatMap(_ => loop(ma))

  def whileDo[A](cond: IO[Boolean])(ma: IO[A]): IO[Unit] = for {
    c <- cond
    _ <- if (c) ma.flatMap(_ => whileDo(cond)(ma)) else IO{()}
  } yield ()

  def doWhile[A](ma: IO[A])(f: A => IO[Boolean]): IO[Unit] = for {
    a <- ma
    ok <- f(a)
    _ <- if (ok) doWhile(ma)(f) else IO{()}
  } yield ()
}