import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by Jeff on 03/04/2015.
 */
@RunWith(classOf[JUnitRunner])
class SchedulingTest extends FunSuite {

  val basePath = "src/main/resources/scheduling"

  test ("Read File") {

    val file = new HeaderFile(s"$basePath/scheduling-tc3.txt")

    val jobs = Scheduling.fromFile(file).jobs
    val taskCount = file.headerAt("Tasks") match {
      case Some(x) => x.toInt
      case None => 0
    }
    assert(jobs.length === taskCount)
    assert(jobs(2) === Job(31, 73))
    assert(jobs(5) === Job(41, 66))

  }

  1 to 5 foreach (i => {

    val file = new HeaderFile(s"$basePath/scheduling-tc$i.txt")
    val scheduling = Scheduling.fromFile(file)

    test("Test Diff " + i ) {

      assert(scheduling.weightedSumByDiff === (file.headerAt("Expected Diff") match {
        case Some(x) => x.toInt
        case None => 0
      }))

    }
    test("Test Ratio " + i ) {

      assert (scheduling.weightedSumByRatio === (file.headerAt("Expected Ratio") match {
        case Some(x) => x.toInt
        case None => 0
      }))

    }
  })

  val file = new HeaderFile(s"$basePath/jobs.txt")
  val scheduling = Scheduling.fromFile(file)

  test("Problem Diff") {

    assert(scheduling.weightedSumByDiff === (file.headerAt("Expected Diff") match {
      case Some(x) => BigInt(x)
      case None => 0
    }))

  }
  test("Problem Ratio") {

    assert (scheduling.weightedSumByRatio === (file.headerAt("Expected Ratio") match {
      case Some(x) => BigInt(x)
      case None => 0
    }))

  }
}
