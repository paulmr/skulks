package skulks.memory

import skulks.util.StateTransform
import scodec.bits.ByteVector

object StackTransform {
	def apply[A](f: Stack => (A, Stack)) = new StateTransform[A, Stack] {
		val op = f
  }
}

trait Stack {
  val sp: Long
  val segment: Segment
	lazy val max = segment.length

  def update(sp: Long = this.sp, segment: Segment = this.segment): Stack

  def pushBytes(bytes: ByteVector) =
    (Unit, update(sp + bytes.length, segment.putBytes(sp, bytes)))
  // don't need to clear the data, just move the stack pointer down
  // however, TODO, it might make it more efficient to clear it so that data can be released
  def popBytes(count: Int) = 
    (segment.getBytes(sp - count, count), update(sp - count))

  override def toString = s"Stack(sp = ${sp}, max = ${max})"
}

// various StackTransform operations that can be combined
object StackOps {
  def pushBytes(bytes: ByteVector) = StackTransform { (s) => s.pushBytes(bytes) }
  def popBytes (count: Int)        = StackTransform { (s) => s.popBytes (count) }
}

class MemStack(val segment: Segment, val sp: Long) extends Stack {
	def update(sp: Long = this.sp, segment: Segment = this.segment): Stack =
		new MemStack(segment, sp)
}
