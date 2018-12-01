package fpgame

case class Lens[S, A](get: S => A, set: (S, A) => S) {
  def |->[B](other: Lens[A, B]): Lens[S, B] =
    Lens(get andThen other.get, (s, b) => set(s, other.set(get(s), b)))
}

object LensTest {
  def main(args: Array[String]): Unit = {
    val player = Lens[GameState, Player](_.player, (s, a) => s.copy(player = a))
    val health = Lens[Player, Int](_.health, (s, a) => s.copy(health = a))
    val position = Lens[Player, Position](_.position, (s, a) => s.copy(position = a))
    val x = Lens[Position, Int](_.x, (s, a) => s.copy(x = a))
    val y = Lens[Position, Int](_.y, (s, a) => s.copy(y = a))


    val p = Player(Position(1, 2), 100)
    val g = GameState(p)
    val g1 = player |-> health set(g, 10)
    val g2 = player |-> position |-> x set(g1, 10)
    val g3 = player |-> position |-> x set(g2, 10)
    println(g3)
  }
}