package fpgame

case class Player(x: Int, y: Int, health: Int)
case class GameState(player: Player)
case class GameMap[A](cells: Vector[Vector[A]]) {
  def at(x: Int, y: Int): A = cells(x)(y)
}

object Game {
  def updateHealth(delta: Int): State[GameState, Int] =
    State(s => {
      val newHealth = s.player.health + delta

      (s.copy(player = s.player.copy(health = newHealth)), newHealth)
    })

  def moveX(dx: Int): State[GameState, Int] =
    State(s => {
      val newX = s.player.x + dx

      (s.copy(player = s.player.copy(x = newX)), newX)
    })

  def moveY(dy: Int): State[GameState, Int] =
    State(s => {
      val newY = s.player.y + dy

      (s.copy(player = s.player.copy(y = newY)), newY)
    })


  def main(args: Array[String]): Unit = {
    val newHealth = for {
      _ <- moveX(1)
      _ <- moveY(2)
      _ <- moveY(2)
      _ <- moveX(100)
      _ <- moveY(2)
      _ <- updateHealth(-10)
      _ <- updateHealth(-100)
    } yield ()
    println(newHealth.run(GameState(Player(0, 0, 100))))
  }
}
