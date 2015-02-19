package skulks.test.memory

import org.scalatest._
import skulks.memory._
import scodec.bits.ByteVector
import skulks.test.TestUtil._

class GlulxStackSpec extends FlatSpec with Matchers {

  "A memstack" should "push and pop the data correctly" in {
    val stk = new MemStack(Segment.empty(1024), 0L)
    val d1 = randomBytes(40)
    val d2 = randomBytes(20)
    false
//    for {
//      s <- stk.pushBytes(d1)
//    } yield s
  }

  "A mempage" should "return stored data" in {
		val offset = 10
		val len = 1024
		val d = randomBytes(len)
		val pg: Page = MemPage.empty
    val exp = MemPage(ByteVector.fill(offset)(0) ++ d.take(Page.size - offset))
		val (newpage, rest) = pg.setBytes(offset, d)
    assert(newpage == exp)
    assert(rest == d.drop(Page.size - offset))
  }

  "A memsegment" should "return stored data" in {
    def storeGetTest(seglen: Int, dlen: Int, off: Int) = {
      val d = randomBytes(dlen)
      val seg: Segment = MemSegment.empty(seglen)
      val actual = seg.putBytes(off, d).getBytes(off, dlen)
      assert(actual == d, s"No match: ${bytesToString(d)} != ${bytesToString(actual)}")
    }
    storeGetTest(256, 3, 0)
    storeGetTest(512, 3, 255)
    storeGetTest(1024 * 5, 567, 355)
  }
}
