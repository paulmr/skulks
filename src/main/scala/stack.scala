package skulks.memory

import skulks.util.StateTransform
import scodec.bits.ByteVector

trait StackTransform[A] extends StateTransform[A, Stack] {
  def build[B](b: B, nextStack: Stack) = StackTransform(b, nextStack)
}

object StackTransform {
  def apply[A](a: A, nextStack: Stack) = new StackTransform[A] {
    val result = (a, nextStack)
  }
  def unit(nextStack: Stack) = apply((), nextStack)
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

  def popBytes(count: Int): StackTransform[ByteVector] =
    // don't need to clear the data, just move the stack pointer down
    // however, TODO, it might make it more efficient to clear it so that data can be released
    StackTransform(data.getBytes(sp, count), new MemStack(data, sp - count))

  def pushBytes(bytes: ByteVector) = StackTransform.unit(new MemStack(data.putBytes(sp, bytes), sp + bytes.length)) 
}
