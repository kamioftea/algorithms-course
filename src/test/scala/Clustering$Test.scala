import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by Jeff on 07/04/2015.
 */
@RunWith(classOf[JUnitRunner])
class Clustering$Test extends FunSuite with Matchers {

  val basePath = "src/main/resources/max-clustering"
  val test_file = new HeaderFile(s"$basePath/c1-tc1.txt")
  val edges = Clustering.edgesFromFile(test_file)

  test("file has all edges") {
    assert(edges.size === 28)
  }

  test("file contains sample nodes") {
    edges should contain(Edge(1, 5, 8))
    edges should contain(Edge(3, 5, 6))
    edges should contain(Edge(3, 8, 15))
    edges should contain(Edge(7, 8, 1))
  }

  (1 to 3).foreach(i => {
    val file = new HeaderFile(s"$basePath/c1-tc$i.txt")

    val rangePattern = "(\\d+)\\.\\.(\\d+)".r
    val singlePattern = "(\\d+)".r

    val (k_min, k_max) = file.headerAt("k").getOrElse("4") match {
      case rangePattern(min, max) => (min.toInt, max.toInt)
      case singlePattern(k) => (k.toInt, k.toInt)
      case _ => throw new RuntimeException("file must specify range of k's")
    }

    (k_min to k_max).foreach(k =>
      test(s"test case $i, k = $k") {
        val expected = file.headerAt(s"expected-$k").map(_.toInt).getOrElse(0)
        Clustering.maxSpacing(file, k) should equal(expected)
      }
    )
  })

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }


  test("problem") {
    val problem_file = new HeaderFile(s"$basePath/clustering1.txt")
    val expected = problem_file.headerAt(s"expected").map(_.toInt).getOrElse(0)
    Clustering.maxSpacing(problem_file, 4) should equal(expected)
  }

  test("get comparison strings 24 bit") {
    val array = Clustering.comparisonInts(24)
    assert(array.size === 24 + 276)
    array should contain(1024)
    array should contain(1025)
    array should contain(1026)

    array should not contain 1023
    array should not contain 1027
  }

  test("get comparison strings 8 bit") {
    val array = Clustering.comparisonInts(8)
    assert(array.size === 8 + 28)
    array should contain(128)
    array should contain(129)
    array should contain(130)

    array should not contain 127
    array should not contain 131
  }

  val test_file2 = new HeaderFile(s"$basePath/clustering_tc1.txt")

  test("can parse line") {
    assert(Clustering.charSeqToInt(test_file2.lines.head.split(' ').map(_.charAt(0))) === 224)
  }

  test("can parse file") {
    val nodes = Clustering.nodesFromFile(test_file2)
    assert(nodes.size === 8)
    nodes should contain(224)
    nodes should contain(160)
    nodes should contain(64)
    nodes should contain(112)
    nodes should contain(255 - 224)
    nodes should contain(255 - 160)
    nodes should contain(255 - 64)
    nodes should contain(255 - 112)
  }

  test("can parse file with duplicates") {
    val dup_test_file = new HeaderFile(s"$basePath/clustering_tc4.txt")
    val nodes = Clustering.nodesFromFile(dup_test_file)
    assert(nodes.size === 8)
    nodes should contain(224)
    nodes should contain(160)
    nodes should contain(64)
    nodes should contain(112)
    nodes should contain(255 - 224)
    nodes should contain(255 - 160)
    nodes should contain(255 - 64)
    nodes should contain(255 - 112)
  }

  (1 to 4).foreach(i => {
    val file = new HeaderFile(s"$basePath/clustering_tc$i.txt")

    test(s"clustering test case $i") {
      val expected = file.headerAt(s"expected").map(_.toInt).getOrElse(0)
      Clustering.maxClusters(file) should equal(expected)
    }
  })


  test("max clusters problem") {
        val problem_file = new HeaderFile(s"$basePath/clustering_big.txt")
        val expected = problem_file.headerAt("expected").map(_.toInt).getOrElse(0)
    time {
        Clustering.maxClusters(problem_file) should equal(expected)
      }
  }
}