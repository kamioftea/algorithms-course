import scala.annotation.tailrec
import scala.io.Source

/**
 *
 * Created by jeff on 04/03/2015.
 */
class HeaderFile(val path: String) {

  private def parseHeaders(lines: List[String]): (Map[String, String], List[String]) = {
    @tailrec
    def doParse(headers: Map[String, String], lines: List[String]): (Map[String, String], List[String]) = lines match {
      case Nil => (headers, Nil)
      case "" :: xs => (headers, xs)
      case x :: xs =>
        val (key, value) = x.span(_ != ':')
        doParse(headers + (key.trim -> value.substring(1).trim), xs)
    }

    doParse(Map(), lines)
  }

  lazy val (headers, lines) = {
    val file = Source.fromFile(path)
    val lines = file.getLines().toList
    file.close()
    parseHeaders(lines)
  }

  def headerAt(key: String): Option[String] =
    if(headers.isDefinedAt(key)) Some(headers(key))
    else None

  def intHeaderAtOrElse(key: String, orElse: Int = 0): Int = this.headerAt(key).map(_.toInt).getOrElse(orElse)
}
