import scala.collection.parallel.ParSeq
import scala.concurrent.{Await, Future, future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

trait FrequencyCommons {
  def secuatialFreq(text: Seq[String]): Map[Char, Int] = {
    def charCount(s: String): Map[Char, Int] =
      s.filter(_.isLetter).toLowerCase groupBy identity mapValues (_.length)

    ((text map charCount) fold Map())(merge)
  }

  def merge(map1: Map[Char, Int], map2: Map[Char, Int]) =
    map1 ++ map2.map{ case (k,v) => k -> (v + map1.getOrElse(k,0)) }
}

object Frequency extends FrequencyCommons {

  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] = {
    require(numWorkers > 0)

    val numberOfLines: Int = texts.length
    val chunkSize =
      if(numberOfLines < numWorkers) 1
      else math.ceil(texts.length / (numWorkers*1.0)).toInt

    val futures: Iterator[Future[Map[Char, Int]]] =
      for(textsChunk <- texts.grouped(chunkSize))
        yield Future { secuatialFreq(textsChunk) }

    val futuresResult: Iterator[Map[Char, Int]] =
      Await.result(Future.sequence(futures), Duration.Inf)

    (futuresResult fold Map())(merge)
  }
}

object FrequencyParSeq extends FrequencyCommons {

  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] = {
    require(numWorkers > 0)

    val numberOfLines: Int = texts.length
    val chunkSize: Int =
      if(numberOfLines < numWorkers) 1
      else math.ceil(texts.length / (numWorkers*1.0)).toInt

    val parSeq: ParSeq[Seq[String]] = texts.grouped(chunkSize).toSeq.par

    ((parSeq map secuatialFreq) fold Map())(merge)
  }
}
