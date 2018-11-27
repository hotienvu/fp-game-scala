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

  def updateHealth(): Game[Unit] =
    StateT(s => for {
      delta <- IO.getLine("Enter health delta: ").map(_.toInt)
      (newState, newHealth) <- IO {
        val newHealth = s.player.health + delta
        (s.copy(player = s.player.copy(health = newHealth)), newHealth)
      }
      _ <- IO.putStrln(s"Player new health: $newHealth")
    } yield (newState, ()))

  def moveX(): Game[Unit] =
    StateT(s => for {
      dx <- IO.getLine("Enter dx: ").map(_.toInt)
      (newState, newX) <- IO {
        val newX = s.player.x + dx

        (s.copy(player = s.player.copy(x = newX)), newX)
      }
      _ <- IO.putStrln(s"Player new position: ($newX, ${newState.player.y})")
    } yield (newState, ()))

  def moveY(): Game[Unit] =
    StateT(s => for {
      dy <- IO.getLine("Enter dy: ").map(_.toInt)
      (newState, newY) <- IO {
        val newY = s.player.y + dy

        (s.copy(player = s.player.copy(y = newY)), newY)
      }
      _ <- IO.putStrln(s"Player new position: (${newState.player.x}, $newY)")
    } yield (newState, ()))

  def printPlayerState(): Game[Unit] =
    StateT(s => IO.putStrln(s"Player state: ${s.player}").map(_ => (s, ())))


  def main(args: Array[String]): Unit = {
    val play = for {
      _ <- moveX()
      _ <- moveY()
      _ <- updateHealth()
    } yield ()
    println(play.run(GameState(Player(0, 0, 100))).run)
  }
}
