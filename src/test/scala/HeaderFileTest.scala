import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by jeff on 04/03/2015.
 */
@RunWith(classOf[JUnitRunner])
class HeaderFileTest extends FunSuite {

  val basePath = "src/main/resources/"

  val file = new HeaderFile(basePath + "twosum-tc1.txt")

  test("header exists") {
    assert(file.headers.isDefinedAt("expected"))
  }

  test("header value") {
    assert(file.headers("expected") === "3")
  }

  test("headerAt exists")
  {
    assert(file.headerAt("expected") === Some("3"))
  }

  test("headerAt missing")
  {
    assert(file.headerAt("not-set") === None)
  }

  test("lines match") {
    assert(file.lines === List(
      "-10001",
      "1",
      "2",
      "-10001"
    ))
  }
}
