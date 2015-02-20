package skulks.util

trait StateTransform[A, S] {
  val result: (A, S)
  def value = result._1
  def next  = result._2
  def build[B](b: B, newstate: S): StateTransform[B, S]
  def map[B](f: A => B): StateTransform[B, S] = build(f(value), next) 
}

