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

    def test(choice: String): Game[Unit] = choice match {
      case "q" => IO.putStrln("Quitting...").toStateT
      case "1" => updateHealth()
      case "2" => moveX()
      case "3" => moveY()
      case "4" => printPlayerState()
      case _ => IO.putStrln("Unknown command").toStateT
    }


    def gameLoop: Game[Unit] = for {
      choice <- inputMenu("game 2").toStateT
      _ <- test(choice)
      _ <- if (choice == "q") IO{()}.toStateT else gameLoop
    } yield ()

    gameLoop.run(gameState).run
  }
}