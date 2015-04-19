import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by Jeff on 19/04/2015.
 */
@RunWith(classOf[JUnitRunner])
class Knapsack$Test extends FunSuite with Matchers {
  val basePath = "src/main/resources/knapsack"
  val test_file_1 = new HeaderFile(s"$basePath/tc1.txt")
  val items = Knapsack.itemsFromFile(test_file_1)

  test("has correct item count") {
    assert(items.size === test_file_1.intHeaderAtOrElse("items"))
  }

  test("file contains correct items") {
    items should contain(Item(3, 4))
    items should contain(Item(2, 3))
    items should contain(Item(4, 2))
    items should contain(Item(4, 3))
  }
  
  test("maxValue 1")
  {
    assert(Knapsack.maxValue(test_file_1) === test_file_1.intHeaderAtOrElse("expected"))
  }

  List("tc2", "knapsack1", "knapsack_big").foreach(filename => {
    val test_file = new HeaderFile(s"$basePath/$filename.txt")

    test(s"maxValue $filename")
    {
      assert(Knapsack.maxValue(test_file) === test_file.intHeaderAtOrElse("expected"))
    }
  })
}
