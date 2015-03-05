import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by Jeff on 05/03/2015.
 */
@RunWith(classOf[JUnitRunner])
class MaintainMedianTest extends FunSuite {
  val basePath = "src/main/resources/maintain-median"

  1 to 6 foreach (i => {
    test("Test Case " + i ) {
      val file = new HeaderFile(s"$basePath/mm-tc$i.txt")
      assert(MaintainMedian.sumMedians(file.lines.map(_.toInt)) % 10000 === (file.headerAt("expected") match {
        case Some(x) => x.toInt
        case None => -1
      }))
    }
  })

  test("Problem " ) {
    val file = new HeaderFile(s"$basePath/Median.txt")
    assert(MaintainMedian.sumMedians(file.lines.map(_.toInt)) % 10000 === (file.headerAt("expected") match {
      case Some(x) => x.toInt
      case None => -1
    }))
  }
}
