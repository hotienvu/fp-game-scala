package fpgame

case class State[S, A](run: S => (S, A)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (s1, a) = run(s)
    f(a).run(s1)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (s, a))
}

case class StateT[M[_], S, A](run: S => M[(S, A)]) {
  def flatMap[B](f: A => StateT[M, S, B])(implicit M: Monad[M]): StateT[M, S, B] = StateT(s => {
    val ma = run(s)
    M.flatMap(ma) { case (s1, a) => f(a).run(s1) }
  })

  def map[B](f: A => B)(implicit M: Monad[M]): StateT[M, S, B] = flatMap(a => StateT.unit(f(a)))
}

object StateT {
  def unit[M[_], S, A](a: A)(implicit M: Monad[M]): StateT[M, S, A] = StateT(s => M.unit((s, a)))
}

