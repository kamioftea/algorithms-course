import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by jeff on 04/03/2015.
 */
@RunWith(classOf[JUnitRunner])
class TwoSumTest extends org.scalatest.FunSuite {

  val basePath = "src/main/resources/twosum/"

  1 to 5 foreach (i => {
    test("Test Case " + i ) {
      val file = new HeaderFile(s"${basePath}twosum-tc$i.txt")
      assert (TwoSum.fromFile(file).overRange(-10000, 10000) === (file.headerAt("expected") match {
        case Some(x) => x.toInt
        case None => 0
      }))
    }
  })

  test("Problem") {
    val file = new HeaderFile(s"${basePath}2sum.txt")
    assert (TwoSum.fromFile(file).overRange(-10000, 10000) === (file.headerAt("expected") match {
      case Some(x) => x.toInt
      case None => 0
    }))
  }

}
