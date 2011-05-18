package scaladays



import collection.parallel._
import collection.mutable.ArrayBuffer



class ParString(val str: String)
extends immutable.ParSeq[Char] {
  
  def apply(i: Int) = str.charAt(i)
  
  def length = str.length
  
  def seq = new collection.immutable.WrappedString(str)
  
  def splitter = new ParStringSplitter(str, 0, str.length) with SCPI
  
  type SCPI = SignalContextPassingIterator[ParStringSplitter]
  
  class ParStringSplitter(private var s: String, private var i: Int, private val ntl: Int)
  extends Splitter[Char] with ParIterator {
  self: SCPI =>
    final def hasNext = i < ntl
    final def next = {
      val r = s.charAt(i)
      i += 1
      r
    }
    def remaining = ntl - i
    def dup = new ParStringSplitter(s, i, ntl) with SCPI
    def split = {
      val rem = remaining
      if (rem >= 2) psplit(rem / 2, rem - rem / 2)
      else Seq(this)
    }
    def psplit(sizes: Int*): Seq[ParIterator] = {
      val splitted = new ArrayBuffer[ParStringSplitter]
      for (sz <- sizes) {
        val next = (i + sz) min ntl
        splitted += new ParStringSplitter(s, i, next) with SCPI
        i = next
      }
      splitted
    }
  }
  
}


object Global {
  val par = Option(System.getProperty("par")).map(_.toInt)
}


object SeqCharCount extends testing.Benchmark {
  
  val txt = "A short text..." * 500000
  val ps = new ParString(txt)
  
  def run() {
    txt.foldLeft(0)((x, y) => x + 1)
  }
  
}


object ParCharCount extends testing.Benchmark {
  
  tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Global.par.get)
  
  val txt = "A short text..." * 500000
  val ps = new ParString(txt)
  
  def run() {
    ps.aggregate(0)((x, y) => x + 1, _ + _)
    //ps.foldLeft(0)((x, y) => x + 1)
  }
  
}


object SeqWordCount extends testing.Benchmark {
  
  val txt = "A short text...  " * 500000
  val ps = new ParString(txt)
  
  def run() {
    val wc = txt.foldLeft((0, true)) {
      case ((wc, _), ' ') => (wc, true)
      case ((wc, true), x) => (wc + 1, false)
      case ((wc, false), x) => (wc, false)
    }
    println(wc)
  }
  
}


object ParWordCount extends testing.Benchmark {
  
  tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Global.par.get)
  
  val txt = "A short text..." * 500000
  val ps = new ParString(txt)
  
  def run() {
    ps.aggregate((false, -1, false))({
      
    }, {
      case ((ls, lwc, linner), (rinner, rwc, rs)) if linner || rinner => (ls, lwc + rwc, rs)
      case ((ls, lwc, false), (false, rwc, rs)) if lwc > 0 && rwc > 0 => (ls, lwc + rwc - 1, rs)
      case ((ls, lwc, _), (_, rwc, rs)) => (ls, lwc + rwc, rs)
    })
  }
  
}








