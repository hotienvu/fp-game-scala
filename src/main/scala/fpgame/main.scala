package fpgame

import scala.io.StdIn

object Main {
  import Game._

  def main(args: Array[String]): Unit = {
    def printMainScreen(gameTitle: String) = IO.putStrln(
      s"""
        | $gameTitle
        | Enter your choice:
        | 1. Print player state
        | 2. Print game map
        | 3. Update health
        | 4. Move player horizontal
        | 5. Move player vertical
        | 6. Pickup items at current cell
        | 7. Drop items at current cell
        | 8. Fight monster at current cell
        | q. exit
      """.stripMargin)

    def inputMenu(gameTitle: String) = for {
      _ <- printMainScreen(gameTitle)
      choice <- IO.getLine("Enter your choice:")
    } yield choice

    def update(choice: String): Game[Unit] = choice match {
      case "1" => printPlayerState()
      case "2" => printGameMap()
      case "3" => updateHealth()
      case "4" => moveX()
      case "5" => moveY()
      case "6" => pickupItems()
      case "7" => dropItems()
      case "8" => fight()
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
    val RG = Foe("Rock Golem", Position(0, 0), 1, 1, 1)
    val FT = Friend("Friendly Tortoise", Position(0, 0), 1, 1, 1)
    val map = GameMap(Vector(
      Vector(FreeCell(items = Set(RustySword), players = Set(RG, FT)), BlockedCell, BlockedCell),
      Vector(FreeCell(items = Set(CrackedShield), players = Set.empty), BlockedCell, BlockedCell),
      Vector(FreeCell(items = Set(RustySword), players = Set.empty), BlockedCell, BlockedCell)
    ))

    val gameState = GameState(Player("Link", Position(0, 0), health = 10, attack = 2, defend = 1), map = map)
    gameLoop.run(gameState).run
  }
}