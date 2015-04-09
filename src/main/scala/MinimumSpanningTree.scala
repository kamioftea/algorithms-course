import scala.annotation.tailrec

/**
 *
 * Created by Jeff on 04/04/2015.
 */
object MinimumSpanningTree {
  val UNSEEN_COST = 100000

  def buildHeap(graph: Graph): LookupHeap = {

    val (heap, lookup, reverseLookup, _) = graph.vertices.tail.foldLeft((Vector[Int](), Map[Int, Int](), Map[Int, Int](), 1))((acc, vertex) => acc match {
      case (h, l, rl, count) =>
        if (count >= graph.vertices.length) (h, l, rl, count)
        else {

          @tailrec
          def getMinCost(edges: List[Edge], cost: Int = UNSEEN_COST): Int = edges match {
            case Nil => cost
            case edge :: xs =>
              if ((edge.a == 0 || edge.b == 0) && edge.cost < cost)
                getMinCost(xs, edge.cost)
              else
                getMinCost(xs, cost)
          }

          (h :+ getMinCost(vertex.edges), l ++ Map(count -> (count - 1)), rl ++ Map((count - 1) -> count), count + 1)
        }
    })

    LookupHeap(heap, lookup, reverseLookup).build
  }

  def primsOverallCost(graph: Graph): BigInt = {
    @tailrec
    def mstIter(explored: Set[Int], unexplored: LookupHeap, costAcc: BigInt): BigInt =
      if (unexplored == LookupHeap.empty) costAcc
      else {
        val (bestVertex, cost, reducedUnexplored) = unexplored.pop()

        val updatedUnexplored = updateVertexCosts(graph, bestVertex, reducedUnexplored)

        def popIter(heap: LookupHeap, prev: Int): Unit = heap match {
          case LookupHeap.empty =>
          case h =>
            val (_, cost, h1) = h.pop()
            if(cost < prev)
            {
              println(updatedUnexplored)
              println(heap)
              println(prev)
              throw new RuntimeException("Heap invariant failed")
            }
            popIter(h1, cost)
        }

        popIter(updatedUnexplored, - 1000000)

        mstIter(explored + bestVertex, updatedUnexplored, costAcc + cost)
      }

    mstIter(Set(0), buildHeap(graph), 0)
  }

  def updateVertexCosts(graph: Graph, bestVertex: Int, reducedUnexplored: LookupHeap): LookupHeap = {
    graph(bestVertex).edges.foldLeft(reducedUnexplored)((heap, edge) => {
      val otherVertex = if (edge.a == bestVertex) edge.b else edge.a

      if (heap.isDefinedAt(otherVertex) && edge.cost < heap(otherVertex)) {
        heap
          .delete(otherVertex)
          .insert(otherVertex, edge.cost)
      }
      else heap
    })
  }
}

case class Edge(a: Int, b: Int, cost: Int)

case class Vertex(edges: List[Edge] = List())

object Vertex {
  val empty = Vertex()
}

case class Graph(vertices: Vector[Vertex] = Vector()) {
  def addEdge(edge: Edge): Graph =
    Graph(vertices
      .updated(edge.a, Vertex(edge :: vertices(edge.a).edges))
      .updated(edge.b, Vertex(edge :: vertices(edge.b).edges)))

  def apply(i: Int): Vertex = vertices(i)
}

object Graph {
  val empty = Graph()

  def fromFile(file: HeaderFile): Graph = {
    val size = file.headerAt("Nodes") match {
      case Some(x) => x.toInt
      case None => 0
    }
    val edgelessGraph = Graph(Vector.fill(size)(Vertex.empty))

    file.lines.foldLeft(edgelessGraph)((graph, line) => {
      val edge = line.split(" ") map (_.toInt) match {
        case Array(a, b, cost, _*) =>
          val indexing = file.headerAt("Indexing").map(_.toInt).getOrElse(1)
          Edge(a - indexing, b - indexing, cost) // 1-indexed => 0-indexed
        case _ => throw new IllegalArgumentException("expects string in '\\d+ \\d+ \\d+' form")
      }

      graph.addEdge(edge)
    })
  }
}
