package skulks.test

import scodec.bits.ByteVector
import scala.util.Random

object TestUtil {
  def bytesToString(b: ByteVector) =
    b.take(10).toArray.map(_.toString).mkString(",") +
      (if(b.length > 10) "..." else "")

	def randomBytes(sz: Int, rnd: Random = Random): ByteVector = {
		val arr = new Array[Byte](sz)
		rnd.nextBytes(arr)
		ByteVector(arr)
	}
}
