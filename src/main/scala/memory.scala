package skulks.memory

import scala.annotation.tailrec

import skulks.error._
import scodec.bits.ByteVector

trait Page {
  // returns the remainder in addition to the relavent value
  def getBytes(start: Int, count: Int): (ByteVector, Int)
  def setBytes(start: Int, bytes: ByteVector): (Page, ByteVector)
}

object Page {
  val size = 256
  // default impl
  lazy val empty: Page = MemPage.empty
}

case class MemPage(data: ByteVector) extends Page {
  require(data.length == Page.size,
    s"Invalid page size for data.length = ${data.length} (page size should be ${Page.size})")

  def getBytes(start: Int, count: Int): (ByteVector, Int) = {
    val bytes = data.drop(start).take(count)
    (bytes, count - bytes.size)
  }
  def setBytes(start: Int, bytes: ByteVector): (Page, ByteVector) = {
    val (fits, rest) = bytes.splitAt(Page.size - start)
    (MemPage(data.patch(start, fits)), rest)
  }
}

object MemPage {
  lazy val empty = MemPage(ByteVector.fill(Page.size)(0))
}

trait Segment {
  import Segment._
  val length: Long
  def getPage(pgnum: Int): Page
  def setPage(pgnum: Int, replaceWith: Page): Segment
  def getBytes(addr: Long, count: Int): ByteVector = {
    assert(addr + count <= length)
    @tailrec
    def go(addr: Long, count: Int, sofar: ByteVector): ByteVector = {
      if(count == 0) sofar
      else {
        val (pg, offset) = addr2page(addr)
        val (bytes, remaining) = getPage(pg).getBytes(offset, count)
        // TODO -> this ++ operation is probably more expensive than it needs
        // to be
        go(addr + bytes.length, remaining, sofar ++ bytes)
      }
    }
    go(addr, count, ByteVector.empty)
  }
  def putBytes(addr: Long, bytes: ByteVector): Segment = {
    def go(addr: Long, bytes: ByteVector, seg: Segment): Segment = {
      if(bytes.length == 0) seg
      else {
        val (pgnum, offset) = addr2page(addr)
        val (newpg, remaining) = getPage(pgnum).setBytes(offset, bytes)
        go(addr + (bytes.length - remaining.length), remaining, seg.setPage(pgnum, newpg))
      }
    }
    go(addr, bytes, this)
  }
}

object Segment {
  // default impl
  def empty(sz: Int): Segment = MemSegment.empty(sz)
  def addr2page(addr: Long) = (
    ((addr / Page.size).toInt, (addr % Page.size).toInt)
  )
}

case class MemSegment(data: Vector[Page]) extends Segment {
  lazy val length: Long = data.length * Page.size
  def getPage(pgnum: Int) = data(pgnum)
  def setPage(pgnum: Int, replaceWith: Page) =
    MemSegment(data.updated(pgnum,  replaceWith))
  override def toString = s"MemSegment(${data.length} page(s))"
}

object MemSegment {
  def empty(sz: Int) = MemSegment(Vector.fill(sz / Page.size)(MemPage.empty))
}
