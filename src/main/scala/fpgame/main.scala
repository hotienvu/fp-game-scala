package fpgame

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    def printMainScreen(gameTitle: String) = IO.putStrln(
      s"""
        | $gameTitle
        | Enter your choice:
        | 1. Update health
        | 2. Move player
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

    game2.run
  }
}