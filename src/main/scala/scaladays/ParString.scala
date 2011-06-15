package scaladays



import collection.parallel._
import collection.mutable.ArrayBuffer


/**
 *  How to run?
 *  -----------
 *  
 *  Start this project in SBT, then just type:
 *  
 *    bench -Dpar=1 scaladays.<bench-name> 10 1
 *  
 *  where <bench-name> is the name of one of the benchmark classes below.
 */


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
  
  def vowel(c: Char) = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'
  
  def genNames(len: Int, names: Seq[String]): Seq[String] = if (len == 0) names else
    (for (s <- genNames(len - 1, names); c <- 'a' to 'z') yield {
      if (s.length == 0) s + c
      else if (vowel(s.last) && !vowel(c)) s + c
      else if (!vowel(s.last) && vowel(c)) s + c
      else s
    }).distinct
  
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
  
  val txt = "A short text...  " * 250000
  val ps = new ParString(txt)
  
  def run() {
    val wc = txt.foldLeft((0, true)) {
      case ((wc, _), ' ') => (wc, true)
      case ((wc, true), x) => (wc + 1, false)
      case ((wc, false), x) => (wc, false)
    }
    //println(wc)
  }
  
}


object ParWordCount extends testing.Benchmark {
  
  tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Global.par.get)
  
  val txt = "A short text...  " * 250000
  val ps = new ParString(txt)
  
  def run() {
    val wc = ps.aggregate((0, 0, 0))({
      case ((ls, 0, _), ' ')   => (ls + 1, 0, ls + 1)
      case ((ls, 0, _), c)     => (ls, 1, 0)
      case ((ls, wc, rs), ' ') => (ls, wc, rs + 1)
      case ((ls, wc, 0), c)    => (ls, wc, 0)
      case ((ls, wc, rs), c)   => (ls, wc + 1, 0)
    }, {
      case ((0, 0, 0), res) => res
      case (res, (0, 0, 0)) => res
      case ((lls, lwc, 0), (0, rwc, rrs)) => (lls, lwc + rwc - 1, rrs)
      case ((lls, lwc, _), (_, rwc, rrs)) => (lls, lwc + rwc, rrs)
    })
    //println(wc)
  }
  
}


object ParWordCountOptimized extends testing.Benchmark {
  
  tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Global.par.get)
  
  val txt = "A short text...  " * 250000
  val ps = new ParString(txt)
  
  def run() {
    val wc = ps.aggregate((0, 0, 0))({ (x, y) =>
      if (x._2 > 0) {
        if (y != ' ') (x, y) match {
          case ((ls, wc, 0), c)    => (ls, wc, 0)
          case ((ls, wc, rs), c)   => (ls, wc + 1, 0)
        } else x match {
          case (ls, wc, rs) => (ls, wc, rs + 1)
        }
      } else (x, y) match {
        case ((ls, 0, _), ' ')   => (ls + 1, 0, ls + 1)
        case ((ls, 0, _), c)     => (ls, 1, 0)
      }
    }, {
      case ((0, 0, 0), res) => res
      case (res, (0, 0, 0)) => res
      case ((lls, lwc, 0), (0, rwc, rrs)) => (lls, lwc + rwc - 1, rrs)
      case ((lls, lwc, _), (_, rwc, rrs)) => (lls, lwc + rwc, rrs)
    })
    //println(wc)
  }
  
}


object SeqFindPairs extends testing.Benchmark {
  
  val names = Global.genNames(3, Array("")).filter(_.length > 1)
  val surnames = Global.genNames(4, Array("")).filter(_.length > 2)
  
  def run() {
    val results = for {
      s <- surnames
      n <- names
      if s endsWith n
    } yield (n, s);
    //println(results)
  }
  
}


object ParFindPairs extends testing.Benchmark {
  
  tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Global.par.get)
  
  val names = Global.genNames(3, Array("")).filter(_.length > 1)
  val surnames = Global.genNames(4, Array("")).filter(_.length > 2)
  
  def run() {
    val results = for {
      s <- surnames.par
      n <- names.par
      if s endsWith n
    } yield (n, s);
    //println(results)
  }
  
}


object SeqGenNames extends testing.Benchmark {
  def vowel(c: Char) = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'
  
  def genNames(len: Int, names: Seq[String]): Seq[String] = if (len == 0) names else
    for (s <- genNames(len - 1, names); c <- 'a' to 'z') yield {
      if (s.length == 0) s + c
      else if (vowel(s.last) && !vowel(c)) s + c
      else if (!vowel(s.last) && vowel(c)) s + c
      else s
    }
  
  def run() {
    genNames(5, Array(""))
  }
}


object ParGenNames extends testing.Benchmark {
  
  tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Global.par.get)
  
  def vowel(c: Char) = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'
  
  def genNames(len: Int, names: ParSeq[String]): ParSeq[String] = if (len == 0) names else
    for (s <- genNames(len - 1, names); c <- 'a' to 'z') yield {
      if (s.length == 0) s + c
      else if (vowel(s.last) && !vowel(c)) s + c
      else if (!vowel(s.last) && vowel(c)) s + c
      else s
    }
  
  def run() {
    genNames(5, mutable.ParArray(""))
  }
}


class ParString2(val str: String)
extends immutable.ParSeq[Char]
   with ParSeqLike[Char, ParString2, collection.immutable.WrappedString]
{
  
  def apply(i: Int) = str.charAt(i)
  
  def length = str.length
  
  def seq = new collection.immutable.WrappedString(str)
  
  def splitter = new ParStringSplitter(str, 0, str.length) with SCPI
  
  protected[this] override def newCombiner: Combiner[Char, ParString2] = new ParStringCombiner
  
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


private class ParStringCombiner extends Combiner[Char, ParString2] {
  var sz = 0
  val chunks = new ArrayBuffer[StringBuilder] += new StringBuilder
  var lastc = chunks.last
  
  def size: Int = sz
  
  def +=(elem: Char): this.type = {
    lastc += elem
    sz += 1
    this
  }
  
  def clear = {
    chunks.clear
    chunks += new StringBuilder
    lastc = chunks.last
    sz = 0
  }
  
  def result: ParString2 = {
    val rsb = new StringBuilder
    for (sb <- chunks) rsb.append(sb)
    new ParString2(rsb.toString)
  }
  
  def combine[U <: Char, NewTo >: ParString2](other: Combiner[U, NewTo]) = if (other eq this) this else {
    val that = other.asInstanceOf[ParStringCombiner]
    sz += that.sz
    chunks ++= that.chunks
    lastc = chunks.last
    this
  }
}


object ParCharFilter extends testing.Benchmark {
  
  tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Global.par.get)
  
  val txt = "A short text..." * 500000
  val ps = new ParString2(txt)
  
  def run() {
    ps.filter(_ != ' ')
  }
  
}


