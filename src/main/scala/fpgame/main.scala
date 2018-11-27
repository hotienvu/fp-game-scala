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
        | 4. Print player state
        | q. exit
      """.stripMargin)

    def inputMenu(gameTitle: String) = for {
      _ <- printMainScreen(gameTitle)
      choice <- IO.getLine("Enter your choice:")
    } yield choice

    val gameState = GameState(Player(0, 0, 100))
    val game2: IO[Unit] = IO.doWhile(inputMenu("game 2")) {
      case "1" => IO { true }
      case "q" => IO { false }
    }

    def update(choice: String): Game[Unit] = choice match {
      case "1" => updateHealth()
      case "2" => moveX()
      case "3" => moveY()
      case "4" => printPlayerState()
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

    gameLoop.run(gameState).run
    inputMenu("game 2").toStateT
      .flatMap(choice => update(choice))
      .flatMap(_ => StateT.get)
      .flatMap(gs => IO.putStrln("Player died. Quitting...").toStateT)
  }
}