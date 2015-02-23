package skulks.util

trait StateTransform[A, S] {
	val op: S => (A, S)
	def apply(s: S) = op(s)
  def map[B](f: A => B) =
    StateTransform[B, S]((s) => { val (a, newstate) = op(s); (f(a), newstate) })
  def flatMap[B](f: A => StateTransform[B, S]) =
    StateTransform[B, S]((s) => { val (a, newstate) = op(s); f(a)(newstate) })
  def foreach(f: A => Unit) =
    StateTransform[Unit, S] { (s) =>
      val (a, newstack) = op(s)
      f(a)
      println(s"[$a] : $f ${f(a)} foreach")
      ((), newstack)
    }
}

object StateTransform {
  def apply[A, S](f: (S) => (A, S)) = new StateTransform[A, S] {
    val op = f
  }
}
