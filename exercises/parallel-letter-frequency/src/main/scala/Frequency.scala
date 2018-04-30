import scala.collection.parallel.ParSeq

object Frequency {
  def secuatialFreq(text: Seq[String]): Map[Char, Int] = {
    def charCount(s: String): Map[Char, Int] =
      s.filter(_.isLetter).toLowerCase groupBy identity mapValues (_.length)

    ((text map charCount) fold Map())(merge)
  }
  private def merge(map1: Map[Char, Int], map2: Map[Char, Int]) =
    map1 ++ map2.map{ case (k,v) => k -> (v + map1.getOrElse(k,0)) }

  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] = {
    val numberOfLines: Int = texts.length
    val chunkSize: Int =
      if(numberOfLines < numWorkers) 1
      else math.ceil(texts.length / (numWorkers*1.0)).toInt
    val parSeq: ParSeq[Seq[String]] = texts.grouped(chunkSize).toSeq.par

    ((parSeq map secuatialFreq) fold Map())(merge)
  }
}
