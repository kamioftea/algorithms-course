import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by Jeff on 03/05/2015.
 */
@RunWith(classOf[JUnitRunner])
class TSPGraphTest extends FunSuite with Matchers {

  val basePath = "src/main/resources/tsp"

  val test_file_1 = new HeaderFile(s"$basePath/tsp-tc1.txt")
  val test_file_2 = new HeaderFile(s"$basePath/tsp-tc2.txt")
  val test_file_3 = new HeaderFile(s"$basePath/tsp-tc3.txt")
  val test_file_4 = new HeaderFile(s"$basePath/tsp-tc4.txt")
  val problem_file = new HeaderFile(s"$basePath/tsp.txt")

  val testTspGraph = TSPGraph.fromFile(test_file_1)

  test("has correct item count") {
    assert(testTspGraph.cities.length === test_file_1.intHeaderAtOrElse("Points"))
  }

  test("file contains correct items") {
    testTspGraph.cities should contain(City(0.0f, 0.0f))
    testTspGraph.cities should contain(City(0.0f, 2.0f))
    testTspGraph.cities should contain(City(0.0f, 4.0f))
    testTspGraph.cities should contain(City(0.0f, 6.0f))
  }

  val files = Vector(test_file_1, test_file_2, test_file_3, test_file_4, problem_file)
  for ( i <- 0 to files.length - 1) {
    test(s"Test Case: ${files(i).path}") {
      val graph = TSPGraph.fromFile(files(i))
      assert(graph.tsp === files(i).headerAt("Expected").map(_.toInt).getOrElse(0))
    }
  }
  
}
