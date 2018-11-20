package fpgame

import fpgame.IO

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
      choice <- IO.getLine
    } yield choice

    val game2: IO[Unit] = IO.doWhile(inputMenu("game 2")) {
      case "1" => for {
        _ <- IO.putStr("Update health: ")
        delta <- IO.getLine.map(_.toInt)
//        newHealth <- Game.updateHealth(delta)
        _ <- IO.putStrln(s"Player health: $delta")
      }  yield true
      case "q" => IO { false }
    }

    game2.run
  }
}