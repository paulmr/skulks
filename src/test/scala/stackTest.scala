package skulks.test.memory

import org.scalatest._
import skulks.memory._
import scodec.bits.ByteVector
import skulks.test.TestUtil._
import StackOps._

class GlulxStackSpec extends FlatSpec with Matchers {

  "A memstack" should "push and pop the data correctly" in {
    println("hello?")
    val stk = new MemStack(Segment.empty(1024), 0L)
    val d1 = randomBytes(40)
    val d2 = randomBytes(20)
		// build the multiple steps of the transform into one transform
    val transform = for {
      _  <- pushBytes(d1)
      _  <- pushBytes(d2)
      a2 <- popBytes(d2.length)
      a1 <- popBytes(d1.length)
    } yield {
			(a1, a2)
    }
		// apply the transform to the stack and check the results
		transform(stk) match {
			case ((a1, a2), _) =>
				a1 should equal (d1)
				a2 should equal (d2)
		}
		
		
  }
}
