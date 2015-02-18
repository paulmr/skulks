package skulks.memory

import skulks.error._
import scodec.bits.ByteVector

trait Page {
  // returns the remainder in addition to the relavent value
  def getBytes(start: Int, count: Int): (ByteVector, Int)
  def setBytes(start: Int, bytes: ByteVector): (Page, ByteVector)
}

object Page {
  val size = 256
}

case class MemPage(data: ByteVector) extends Page {
  require(data.length == Page.size,
    s"Invalid page size for data.length = ${data.length} (page size should be ${Page.size})")

  def getBytes(start: Int, count: Int): (ByteVector, Int) = {
    val bytes = data.drop(start).take(count)
    (bytes, count - bytes.size)
  }
    //data.slice(start, start + count)
  def setBytes(start: Int, bytes: ByteVector): (Page, ByteVector) = {
    val (fits, rest) = bytes.splitAt(Page.size - start)
    (MemPage(data.patch(start, fits)), rest)
  }
}

object MemPage {
  val empty = MemPage(ByteVector.fill(Page.size)(0))
}

trait Segment {
  val length: Long
  def getPage(pgnum: Int): Page
  def setPage(pgnum: Int, replaceWith: Page): Segment
//  def getBytes(addr: Long, count: Int): ByteVector =
}

object Segment {
  def addr2page(addr: Long) = (
    ((addr / Page.size).toInt, (addr % Page.size).toInt)
  )
}

case class MemSegment(data: Vector[Page]) {
  lazy val length = data.length * Page.size
  def getPage(pgnum: Int) = data(pgnum)
  def setPage(pgnum: Int, replaceWith: Page) =
    MemSegment(data.updated(pgnum,  replaceWith))
}
