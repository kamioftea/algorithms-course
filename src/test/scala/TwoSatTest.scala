import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by Jeff on 06/05/2015.
 */
@RunWith(classOf[JUnitRunner])
class TwoSatTest extends FunSuite with Matchers {
  val basePath = "src/main/resources/2sat"

  val test_file_1 = new HeaderFile(s"$basePath/tc1.txt")
  val test_file_2 = new HeaderFile(s"$basePath/tc2.txt")

  val testTwoSat = TwoSat.fromFile(test_file_1)

  test("has correct item count") {
    assert(testTwoSat.buckets.size === test_file_1.intHeaderAtOrElse("Clauses") + 1)
  }

  test("buckets contain correct items") {
    testTwoSat.buckets(1).hasClauses.size should equal(3)
    testTwoSat.buckets(1).notClauses.size should equal(0)
    testTwoSat.buckets(1).hasClauses should contain(Clause(Has(1), Has(2)))
    testTwoSat.buckets(1).hasClauses should contain(Clause(Has(1), Has(4)))
    testTwoSat.buckets(1).hasClauses should contain(Clause(Has(1), Not(8)))

    testTwoSat.buckets(2).hasClauses.size should equal(2)
    testTwoSat.buckets(2).notClauses.size should equal(0)
    testTwoSat.buckets(2).hasClauses should contain(Clause(Has(1), Has(2)))
    testTwoSat.buckets(2).hasClauses should contain(Clause(Has(2), Has(3)))

    testTwoSat.buckets(8).hasClauses.size should equal(1)
    testTwoSat.buckets(8).notClauses.size should equal(1)
    testTwoSat.buckets(8).hasClauses should contain(Clause(Has(7), Has(8)))
    testTwoSat.buckets(8).notClauses should contain(Clause(Has(1), Not(8)))
  }

  test("preFilter removes all items from tc1")
  {
    assert(testTwoSat.preFilter.buckets === Map())
  }

  test("preFilter leaves conflicting items in tc2")
  {
    val test_file_2 = new HeaderFile(s"$basePath/tc2.txt")

    val testTwoSat2 = TwoSat.fromFile(test_file_2).preFilter

    assert(testTwoSat2.buckets.size === 2)
    testTwoSat2.buckets(1).hasClauses.size should equal(2)
    testTwoSat2.buckets(1).notClauses.size should equal(2)
    testTwoSat2.buckets(1).hasClauses should contain(Clause(Has(1), Has(2)))
    testTwoSat2.buckets(1).hasClauses should contain(Clause(Has(1), Not(2)))
    testTwoSat2.buckets(1).notClauses should contain(Clause(Not(1), Has(2)))
    testTwoSat2.buckets(1).notClauses should contain(Clause(Not(1), Not(2)))

    testTwoSat2.buckets(2).hasClauses.size should equal(2)
    testTwoSat2.buckets(2).notClauses.size should equal(2)
    testTwoSat2.buckets(2).hasClauses should contain(Clause(Has(1), Has(2)))
    testTwoSat2.buckets(2).notClauses should contain(Clause(Has(1), Not(2)))
    testTwoSat2.buckets(2).hasClauses should contain(Clause(Not(1), Has(2)))
    testTwoSat2.buckets(2).notClauses should contain(Clause(Not(1), Not(2)))
  }

  (1 to 8).foreach (i => {
    val file = new HeaderFile(s"$basePath/tc$i.txt")
    test (s"tc $i") {
      assert(TwoSat.fromFile(file).preFilter.isSatisfiable === (file.headerAt("Expected") == Some("true")))
    }
  })

  (1 to 6).foreach (i => {
    val file = new HeaderFile(s"$basePath/2sat$i.txt")
    test (s"Problem $i") {
      assert(TwoSat.fromFile(file).preFilter.isSatisfiable === (file.headerAt("Expected") == Some("true")))
    }
  })
}

/*
1 2
2 3
3 4
1 4
4 5
6 7
7 8
1 -8
*/
