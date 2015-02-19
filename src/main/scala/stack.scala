package skulks.memory

import scodec.bits.ByteVector

trait StackTransform[A] {
  val op: () => (A, Stack)
  lazy val force = op()
  def value: A = force._1
  def next: Stack = force._2

  def map[B](f: A => B): StackTransform[B] = StackTransform((f(value), next))
}

object StackTransform {
  def apply[A](f: => (A, Stack)) = new StackTransform[A] {
    val op = () => f
  }
}

trait Stack {
  val max: Long
  val sp: Long

  def pushBytes(bytes: ByteVector): StackTransform[Unit]
  def popBytes(count: Int): StackTransform[ByteVector]

  override def toString = s"Stack(sp = ${sp}, max = ${max})"
}

class MemStack(data: Segment, val sp: Long) extends Stack {
  val max = data.length

  def popBytes(count: Int): StackTransform[ByteVector] = StackTransform {
    // don't need to clear the data, just move the stack pointer down
    // however, TODO, it might make it more efficient to clear it so that data can be released
    (data.getBytes(sp, count), new MemStack(data, sp - count))
  }

  def pushBytes(bytes: ByteVector) = StackTransform { 
    ((), new MemStack(data.putBytes(sp, bytes), sp + bytes.length))
  }
}
