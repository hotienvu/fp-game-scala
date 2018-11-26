package fpgame

case class Player(x: Int, y: Int, health: Int)
case class GameState(player: Player)
case class GameMap[A](cells: Vector[Vector[A]]) {
  def at(x: Int, y: Int): A = cells(x)(y)
}

object Game {
  def updateHealth(): StateT[IO, GameState, Int] =
    StateT(s => for {
      delta <- IO.getLine("Enter health delta: ").map(_.toInt)
      (newState, newHealth) <- IO {
        val newHealth = s.player.health + delta
        (s.copy(player = s.player.copy(health = newHealth)), newHealth)
      }
      _ <- IO.putStrln(s"Player new health: $newHealth")
    } yield (newState, newHealth))

  def moveX(): StateT[IO, GameState, Int] =
    StateT(s => for {
      dx <- IO.getLine("Enter dx: ").map(_.toInt)
      (newState, newX) <- IO {
        val newX = s.player.x + dx

        (s.copy(player = s.player.copy(x = newX)), newX)
      }
      _ <- IO.putStrln(s"Player new position: ($newX, ${newState.player.y})")
    } yield (newState, newX))

  def moveY(): StateT[IO, GameState, Int] =
    StateT(s => for {
      dy <- IO.getLine("Enter dy: ").map(_.toInt)
      (newState, newY) <- IO {
        val newY = s.player.y + dy

        (s.copy(player = s.player.copy(y = newY)), newY)
      }
      _ <- IO.putStrln(s"Player new position: (${newState.player.x}, $newY)")
    } yield (newState, newY))


  def main(args: Array[String]): Unit = {
    val play = for {
      _ <- moveX()
      _ <- moveY()
      _ <- updateHealth
    } yield ()
    println(play.run(GameState(Player(0, 0, 100))).run)
  }
}
