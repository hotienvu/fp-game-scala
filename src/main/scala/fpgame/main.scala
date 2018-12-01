package fpgame

import scala.io.StdIn

object Main {
  import Game._

  def main(args: Array[String]): Unit = {
    def printMainScreen(gameTitle: String) = IO.putStrln(
      s"""
        | $gameTitle
        | Enter your choice:
        | 1. Update health
        | 2. Move player horizontal
        | 3. Move player vertical
        | 4. Pickup items at current cell
        | 5. Drop items at current cell
        | 6. Print player state
        | 7. Print game map
        | q. exit
      """.stripMargin)

    def inputMenu(gameTitle: String) = for {
      _ <- printMainScreen(gameTitle)
      choice <- IO.getLine("Enter your choice:")
    } yield choice

    def update(choice: String): Game[Unit] = choice match {
      case "1" => updateHealth()
      case "2" => moveX()
      case "3" => moveY()
      case "4" => pickupItems()
      case "5" => dropItems()
      case "6" => printPlayerState()
      case "7" => printGameMap()
      case "q" => IO{()}.toStateT
      case _ => IO.putStrln("Unknown command").toStateT
    }


    def gameLoop: Game[Unit] = for {
      choice <- inputMenu("game 2").toStateT
      _ <- update(choice)
      state <- StateT.get
      _ <- {
        if (choice == "q") IO.putStrln("Quitting...").toStateT
        else if (state.player.health <= 0) IO.putStrln("Player died. Quitting...").toStateT
        else gameLoop
      }
    } yield ()

    val RustySword = Sword("Rusty sword", 10)
    val CrackedShield = Sword("Cracked shield", 10)
    val map = GameMap(Vector(
      Vector(FreeCell(items = Set(RustySword), players = Set.empty), BlockedCell, BlockedCell),
      Vector(FreeCell(items = Set(CrackedShield), players = Set.empty), BlockedCell, BlockedCell),
      Vector(FreeCell(items = Set(RustySword), players = Set.empty), BlockedCell, BlockedCell)
    ))

    val gameState = GameState(Player(Position(0, 0), 100), map = map)
    gameLoop.run(gameState).run
  }
}