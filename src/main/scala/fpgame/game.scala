package fpgame

sealed trait Character
case class NPC(position: Position, health: Int) extends Character
case class Player(position: Position, health: Int, items: Set[Item] = Set.empty) extends Character

case class Position(x: Int, y: Int)

case class GameState(player: Player, map: GameMap = GameMap())
case class GameMap(cells: Vector[Vector[Cell]] = Vector.empty) {
  def at(x: Int, y: Int): Cell = cells(x)(y)
  def updated(x: Int, y: Int, c: Cell): GameMap =
    GameMap(cells.updated(x, cells(x).updated(y, c)))
}

sealed trait Cell
case class FreeCell(items: Set[Item], players: Set[Character]) extends Cell
case object BlockedCell extends Cell


sealed trait Item
case class Sword(name: String, attack: Int) extends Item
case class Shield(name: String, defend: Int) extends Item


object Game extends Monad[({type f[x] = StateT[IO, GameState, x]})#f] {
  override def unit[A](a: A): StateT[IO, GameState, A] = StateT.unit(a)

  override def flatMap[A, B](ma: StateT[IO, GameState, A])(f: A => StateT[IO, GameState, B]): StateT[IO, GameState, B] = ma flatMap f

  type Game[A] = StateT[IO, GameState, A]

  implicit class LiftedIO[A](val io: IO[A]) {
    def toStateT: StateT[IO, GameState, A] = StateT(s => io.map(a => (s, a)))
  }

  def nothing(): Game[Unit] = IO{()}.toStateT

  private val map_ = Lens[GameState, GameMap](_.map, (s, a) => s.copy(map = a))
  private def cell_(x: Int, y: Int) = Lens[GameMap, Cell](_.at(x, y), (s, a) => s.updated(x, y, a))
  private def freeCell_(x: Int, y: Int) = Lens[GameMap, FreeCell](_.at(x, y).asInstanceOf[FreeCell], (s, a) => s.updated(x, y, a))
  private def cellItems = Lens[FreeCell, Set[Item]](_.items, (s, a) => s.copy(items = a))
  private val player_ = Lens[GameState, Player](_.player, (s, a) => s.copy(player = a))
  private val items_ = Lens[Player, Set[Item]](_.items, (s, a) => s.copy(items = a))
  private val health_ = Lens[Player, Int](_.health, (s, a) => s.copy(health = a))
  private val position_ = Lens[Player, Position](_.position, (s, a) => s.copy(position = a))
  private val x_ = Lens[Position, Int](_.x, (s, a) => s.copy(x = a))
  private val y_ = Lens[Position, Int](_.y, (s, a) => s.copy(y = a))

  def updateHealth(): Game[Unit] = for {
    delta <- IO.getLine("Enter health delta: ").map(_.toInt).toStateT
    _ <- update(player_ |-> health_)(_ + delta)
    _ <- printPlayerState()
  } yield ()

  def moveX(): Game[Unit] = for {
    dx <- IO.getLine("Enter dx: ").map(_.toInt).toStateT
    _ <- update(player_ |-> position_ |-> x_)(_ + dx)
    _ <- printPlayerState()
  } yield ()

  def moveY(): Game[Unit] = for {
    dy <- IO.getLine("Enter dy: ").map(_.toInt).toStateT
    _ <- update(player_ |-> position_ |-> y_)(_ + dy)
    _ <- printPlayerState()
  } yield ()

  def pickupItems(): Game[Unit] = for {
    pos <- get(player_ |-> position_)
    c <- get(map_ |-> cell_(pos.x, pos.y))
    _ <- c match {
      case BlockedCell => nothing()
      case FreeCell(items, _) => foreach(
        update(player_ |-> items_ )(_ ++ items),
        update(map_ |-> freeCell_(pos.x, pos.y) |-> cellItems)(_ -- items)
      )
    }
  } yield()

  def dropItems(): Game[Unit] = for {
    pos <- get(player_ |-> position_)
    items <- get(player_ |-> items_)
    c <- get(map_ |-> cell_(pos.x, pos.y))
    _ <- c match {
      case BlockedCell => nothing()
      case _ : FreeCell => foreach(
        update(player_ |-> items_)(_ -- items),
        update(map_ |-> freeCell_(pos.x, pos.y) |-> cellItems)(_ ++ items)
      )
    }
  } yield ()

  def printPlayerState(): Game[Unit] = for {
    player <- get(player_)
    _ <- liftIO(IO.putStrln(s"Player state: $player"))
  } yield()

  def printGameMap(): Game[Unit] = for {
    m <- get(map_)
    _ <- liftIO(IO {
      m.cells.foreach(row => {
        println(row.mkString("  "))
      })
    })
  } yield()

  private def update[A](lens: Lens[GameState, A])(f: A => A): Game[A] =
    StateT(s => IO {
      val v = f(lens.get(s))
      (lens.set(s, v), v)
    })

  private def get[A](lens: Lens[GameState, A]): Game[A] =
    StateT[IO, GameState, A] (s => IO { (s, lens.get(s)) } )

  private def liftIO[A](io: IO[A]): Game[A] = StateT(s => io.map((s, _)))

  def main(args: Array[String]): Unit = {
    val play = for {
      _ <- moveX()
      _ <- moveY()
      _ <- updateHealth()
    } yield ()
    println(play.run(GameState(Player(Position(0, 0), 100))).run)
  }
}
