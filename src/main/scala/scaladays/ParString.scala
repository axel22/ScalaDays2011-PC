package scaladays



import collection.parallel._
import collection.mutable.ArrayBuffer



class ParString(val str: String)
extends immutable.ParSeq[Char] {
  
  def apply(i: Int) = str.charAt(i)
  
  def length = str.length
  
  def seq = new collection.immutable.WrappedString(str)
  
  def splitter = new ParStringSplitter(0, str.length) with SCPI
  
  type SCPI = SignalContextPassingIterator[ParStringSplitter]
  
  class ParStringSplitter(var i: Int, val ntl: Int)
  extends Splitter[Char] with ParIterator {
  self: SCPI =>
    def hasNext = i < ntl
    def next = {
      val r = str.charAt(i)
      i += 1
      r
    }
    def remaining = ntl - i
    def dup = new ParStringSplitter(i, ntl) with SCPI
    def split = {
      val rem = remaining
      if (rem >= 2) psplit(rem / 2, rem - rem / 2)
      else Seq(this)
    }
    def psplit(sizes: Int*): Seq[ParIterator] = {
      val splitted = new ArrayBuffer[ParStringSplitter]
      for (sz <- sizes) {
        val next = (i + sz) min ntl
        splitted += new ParStringSplitter(i, next) with SCPI
        i = next
      }
      splitted
    }
  }
  
}


object Example {
  
  def main(args: Array[String]) {
    val txt = "A short text..." * 1000000
    val ps = new ParString(txt)
    
    // val characters = ps.aggregate(0)((x, y) => x + 1, _ + _)
    // println(characters)
    
    val s = System.currentTimeMillis
    for (i <- 0 until 50) ps.aggregate(0)((x, y) => x + 1, _ + _)
    //for (i <- 0 until 50) txt.foldLeft(0)((x, y) => x + 1)
    val e = System.currentTimeMillis
    println(e - s)
  }
  
}
