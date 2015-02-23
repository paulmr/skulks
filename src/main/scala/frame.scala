package skulks.memory

import scodec.bits.{BitVector, ByteVector}
import scodec.Decoder
import scodec.codecs.uint32

case class CallStub(
  destType: Long,
  destAddr: Long,
  pc:       Long,
  framePtr: Long
)

object CallStub {
  val decoder = for {
    destType <- uint32
    destAddr <- uint32
    pc       <- uint32
    framePtr <- uint32
  } yield CallStub(destType, destAddr, pc, framePtr)
}

trait FrameStore {
	val stack: Stack
  val fp: Long

	def update(fp: Long = this.fp, stack: Stack = this.stack): FrameStore

  def pushFrameBytes(bytes: ByteVector): (Unit, FrameStore) = {
    // after pushing the bytes, create new FrameStore stack that has fp set to
    // current sp
		val (_, newstack) = stack.pushBytes(bytes)
    ((), update(stack.sp, newstack))
  }
  def popFrameBytes(newfp: Long): (ByteVector, FrameStore) = {
    val framesz = stack.sp - fp
    val (oldframe, newstack) = stack.popBytes(framesz.toInt)
    val callstub = CallStub.decoder.decode(oldframe.bits).require.value
    (oldframe, update(callstub.framePtr, newstack))
  }
}

case class TestFrame(stack: Stack, fp: Long) extends FrameStore {
	def update(fp: Long = this.fp, stack: Stack = this.stack): FrameStore =
		new TestFrame(stack, fp)
}
