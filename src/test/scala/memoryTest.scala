package skulks.test.memory

import org.scalatest._
import skulks.memory._
import scodec.bits.ByteVector
import scala.util.Random

class GlulxMemorySpec extends FlatSpec with Matchers {

	def randomBytes(sz: Int, rnd: Random = Random): ByteVector = {
		val arr = new Array[Byte](sz)
		rnd.nextBytes(arr)
		ByteVector(arr)
	}

  def randomPage: Page = MemPage(randomBytes(Page.size))

  "A page" should "return stored data" in {
		val offset = 10
		val len = 1024
		val d = randomBytes(len)
		val pg = MemPage.empty
    val exp = MemPage(ByteVector.fill(offset)(0) ++ d.take(Page.size - offset))
		pg.setBytes(offset, d) match {
      case (newpage, rest) => 
        assert(newpage == exp)
        assert(rest == d.drop(Page.size - offset))
    }
  }

}
