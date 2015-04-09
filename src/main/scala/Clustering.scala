import scala.annotation.tailrec

/**
 *
 * Created by Jeff on 07/04/2015.
 */
object Clustering {
  def edgesFromFile(file: HeaderFile): List[Edge] =
    file.lines.map(_.split(' ').map(_.trim.toInt) match {
      case Array(a, b, dist, _*) => Edge(a, b, dist)
      case _ => null
    }).filter(_ != null)

  def maxSpacing(file: HeaderFile, k: Int): Int = {
    val nodes = file.headerAt("nodes").getOrElse("0").toInt
    assert(nodes > 0, "file must specify nodes")

    val unionFind = UnionFind.fromSeq(1 to nodes)
    val edges = edgesFromFile(file).sortWith(_.cost < _.cost)

    @tailrec
    def doMaxSpacing(unionFind: UnionFind[Int], edges: List[Edge], clusters: Int): Int = {
      edges match {
        case x :: xs =>
          if (unionFind.find(x.a) == unionFind.find(x.b))
            doMaxSpacing(unionFind, xs, clusters)
          else if (k == clusters)
            x.cost
          else
            doMaxSpacing(unionFind.union(x.a, x.b), xs, clusters - 1)

        case Nil => throw new RuntimeException(s"Reached end of edges with only $clusters / $k clusters found")
      }
    }

    doMaxSpacing(unionFind, edges, nodes)
  }

  def charSeqToInt(bits: Seq[Char]) = bits.foldLeft(0)((acc, bit) => (acc << 1) + (if (bit == '1') 1 else 0))

  def comparisonInts(bits: Int): IndexedSeq[Int] = {
    val one_bit_strings = (0 to bits - 1).map(1 << _)
    val two_bit_strings = for (
      i <- 0 to (bits - 1);
      j <- (i + 1) to (bits - 1)
    ) yield one_bit_strings(i) ^ one_bit_strings(j)

    one_bit_strings ++ two_bit_strings
  }

  def nodesFromFile(file: HeaderFile): List[Int] = {
    val nodes = file.lines.map((line) => charSeqToInt(line.split(' ').map(_.charAt(0))))

    nodes.toSet.toList
  }

  def maxClusters(file: HeaderFile): Int = {
    val nodes = nodesFromFile(file)
    val toCompare = comparisonInts(file.headerAt("bits").map(_.toInt).getOrElse(1))

    val unionFind = nodes.foldLeft(UnionFind.fromSeq(nodes))((uf, node) =>
      toCompare.foldLeft(uf)((unionFind, comparison) =>
        unionFind.union(node, node ^ comparison)
      )
    )

    unionFind.leaders.size
  }
}

