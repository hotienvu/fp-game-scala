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

  val healthL = Lens[Player, Int](_.health, (s, a) => s.copy(health = a))
  val xL = Lens[Player, Int](_.x, (s, a) => s.copy(x = a))
  val yL = Lens[Player, Int](_.y, (s, a) => s.copy(y = a))
  val playerL = Lens[GameState, Player](_.player, (s, a) => s.copy(player = a))

  def updateHealth(): Game[Unit] =
    StateT(s => for {
      delta <- IO.getLine("Enter health delta: ").map(_.toInt)
      (newState, newHealth) <- IO {
        val newHealth = s.player.health + delta
        (playerL |-> healthL set(s, newHealth), newHealth)
      }
      _ <- IO.putStrln(s"Player new health: $newHealth")
    } yield (newState, ()))

  def moveX(): Game[Unit] =
    StateT(s => for {
      dx <- IO.getLine("Enter dx: ").map(_.toInt)
      (newState, newX) <- IO {
        val newX = s.player.x + dx

        (playerL |-> xL set(s, newX), newX)
      }
      _ <- IO.putStrln(s"Player new position: ($newX, ${newState.player.y})")
    } yield (newState, ()))

  def moveY(): Game[Unit] =
    StateT(s => for {
      dy <- IO.getLine("Enter dy: ").map(_.toInt)
      (newState, newY) <- IO {
        val newY = s.player.y + dy

        (playerL |-> yL set(s, newY), newY)
      }
      _ <- IO.putStrln(s"Player new position: (${newState.player.x}, $newY)")
    } yield (newState, ()))

  def printPlayerState(): Game[Unit] =
    StateT(s => IO.putStrln(s"Player state: ${s.player}").map(_ => (s, ())))

  def update[A](lens: Lens[GameState, A])(f: A => A): Game[A] = ???

  def main(args: Array[String]): Unit = {
    val play = for {
      _ <- moveX()
      _ <- moveY()
      _ <- updateHealth()
    } yield ()
    println(play.run(GameState(Player(0, 0, 100))).run)
  }
}
