package object fpgame {
  private[fpgame] type Game[A] = StateT[IO, GameState, A]

  private[fpgame] sealed trait Entity
  private[fpgame] sealed trait Character extends Entity {
    def name: String
    def position: Position
    def health: Int
    def attack: Int
    def defend: Int

  }
  private[fpgame] sealed trait NPC extends Character
  private[fpgame] case class Friend(name: String, position: Position, health: Int, attack: Int, defend: Int) extends NPC
  private[fpgame] case class Foe(name: String, position: Position, health: Int, attack: Int, defend: Int) extends NPC {
    def addHealth(delta: Int): Foe = this.copy(health = health + delta)
  }

  private[fpgame] case class Player(name: String, position: Position, health: Int = 100, attack: Int = 0, defend: Int = 0, items: Set[Item] = Set.empty) extends Character

  private[fpgame] case class Position(x: Int, y: Int)

  private[fpgame] case class GameState(player: Player, map: GameMap = GameMap())
  private[fpgame] case class GameMap(cells: Vector[Vector[Cell]] = Vector.empty) {
    def at(x: Int, y: Int): Cell = cells(x)(y)
    def updated(x: Int, y: Int, c: Cell): GameMap =
      GameMap(cells.updated(x, cells(x).updated(y, c)))
  }

  private[fpgame] sealed trait Cell
  private[fpgame] case class FreeCell(items: Set[Item], players: Set[Character]) extends Cell
  private[fpgame] case object BlockedCell extends Cell


  private[fpgame] sealed trait Item
  private[fpgame] case class Sword(name: String, attack: Int) extends Item
  private[fpgame] case class Shield(name: String, defend: Int) extends Item
}
