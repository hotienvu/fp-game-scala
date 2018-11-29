package fpgame

case class Player(x: Int, y: Int, health: Int)
case class GameState(player: Player)
case class GameMap[A](cells: Vector[Vector[A]]) {
  def at(x: Int, y: Int): A = cells(x)(y)
}


object Game {
  type Game[A] = StateT[IO, GameState, A]

  implicit class LiftedIO[A](val io: IO[A]) {
    def toStateT: StateT[IO, GameState, A] = StateT(s => io.map(a => (s, a)))
  }

  private val healthL = Lens[Player, Int](_.health, (s, a) => s.copy(health = a))
  private val xL = Lens[Player, Int](_.x, (s, a) => s.copy(x = a))
  private val yL = Lens[Player, Int](_.y, (s, a) => s.copy(y = a))
  private val playerL = Lens[GameState, Player](_.player, (s, a) => s.copy(player = a))

  def updateHealth(): Game[Unit] = for {
    delta <- IO.getLine("Enter health delta: ").map(_.toInt).toStateT
    _ <- update(playerL |-> healthL)(_ + delta)
  } yield ()

  def moveX(): Game[Unit] = for {
    dx <- IO.getLine("Enter dx: ").map(_.toInt).toStateT
    _ <- update(playerL |-> xL)(_ + dx)
  } yield ()

  def moveY(): Game[Unit] = for {
    dy <- IO.getLine("Enter dy: ").map(_.toInt).toStateT
    _ <- update(playerL |-> yL)(_ + dy)
  } yield ()

  def printPlayerState(): Game[Unit] =
    StateT(s => IO.putStrln(s"Player state: ${s.player}").map(_ => (s, ())))

  private def update[A](lens: Lens[GameState, A])(f: A => A): Game[Unit] =
    StateT[IO, GameState, A](s => IO {
      val v = f(lens.get(s))
      (lens.set(s, v), v)
    }).flatMap(_ => printPlayerState())

  def main(args: Array[String]): Unit = {
    val play = for {
      _ <- moveX()
      _ <- moveY()
      _ <- updateHealth()
    } yield ()
    println(play.run(GameState(Player(0, 0, 100))).run)
  }
}
