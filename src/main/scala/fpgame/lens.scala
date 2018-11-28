package fpgame

case class Lens[S, A](get: S => A, set: (S, A) => S) {
  def |->[B](other: Lens[A, B]): Lens[S, B] =
    Lens(get andThen other.get, (s, b) => set(s, other.set(get(s), b)))
}

object LensTest {
  def main(args: Array[String]): Unit = {
    val health = Lens[Player, Int](_.health, (s, a) => s.copy(health = a))
    val x = Lens[Player, Int](_.x, (s, a) => s.copy(x = a))
    val y = Lens[Player, Int](_.y, (s, a) => s.copy(y = a))
    val player = Lens[GameState, Player](_.player, (s, a) => s.copy(player = a))


    val p = Player(1, 2, 100)
    val g = GameState(p)
    val g1 = player |-> health set(g, 10)
    val g2 = player |-> x set(g1, 10)
    val g3 = player |-> y set(g2, 10)
    println(g3)
  }
}