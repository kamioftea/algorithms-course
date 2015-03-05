import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by Jeff on 05/03/2015.
 */
@RunWith(classOf[JUnitRunner])
class HeapTest extends FunSuite {
  test("heap") {
    val heap = Heap[Int](_ > _)(Vector(4, 5, 2, 3, 1)).build

    assert((heap.pop() match {
      case None => null
      case Some((i,_)) => i
    }) === 1)

    val vector = {
      def buildVector(v: Vector[Int], h: Heap[Int]): Vector[Int] =
        h.pop() match {
          case None => v
          case Some((i, h1)) => buildVector(v :+ i, h1)
        }
      buildVector(Vector(), heap)
    }

    assert(vector === Vector(1,2,3,4,5))
  }
}
