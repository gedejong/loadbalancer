/**
 * State monad helper methods.
 */
object State {
  def unit[S, A](a: A): State[S, A] = State(s => (s, a))
  def get[S]: State[S, S] = State(s => (s, s))
  def gets[S, A](f: S => A): State[S, A] = State { s: S => (s, f(s)) }
  def put[S](s: S): State[S, Unit] = State(_ => (s, ()))
  def modify[S](ss: S => S): State[S, Unit] =
    for { s <- get[S]; _ <- put(ss(s)) } yield ()
}

/**
 * A very minimal state monad.
 * @param run The method which will change the internal state and return a specific A.
 * @tparam S The state type.
 * @tparam A The return type.
 */
case class State[S, A](run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (s1, a) = run(s)
    f(a).run(s1)
  })

  def runA(s: S): A = run(s)._2
}