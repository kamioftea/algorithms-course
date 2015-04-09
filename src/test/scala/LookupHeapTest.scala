import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by Jeff on 04/04/2015.
 */
@RunWith(classOf[JUnitRunner])
class LookupHeapTest extends FunSuite {
  test("swap") {
    val heap = LookupHeap(Vector(1, 2, 3, 4, 5), Map(1 -> 0, 2 -> 4, 3 -> 2, 4 -> 1, 5 -> 3), Map(0 -> 1, 4 -> 2, 2 -> 3, 1 -> 4, 3 -> 5))
    assert(heap.swap(2, 3) === LookupHeap(Vector(1, 2, 4, 3, 5), Map(1 -> 0, 2 -> 4, 3 -> 3, 4 -> 1, 5 -> 2), Map(0 -> 1, 2 -> 5, 3 -> 3, 1 -> 4, 4 -> 2)))
  }

  test("swap 2") {
    val heap = LookupHeap(Vector(1, 2, 3, 4, 5), Map(1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4), Map(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5))
    assert(heap
      .swap(1, 4)
      .swap(0, 2)
      .swap(1, 3)
      .swap(3, 4)
      .swap(1, 3)
      .swap(0, 2) === heap)
  }

  test ("insert") {
    val vector = Vector(23,17,8, 5, 3, 2, 1)
    val (heap, _) = vector.foldLeft((LookupHeap(), 0))((acc, weight) => acc match {
      case (h, count) => (h.insert(count, weight), count + 1)
    })

    val firstRoot = (Math.floor(heap.heap.length / 2) - 1).toInt

    (firstRoot to 0 by -1).foreach(i => {
      assert(heap.heap(i) <= heap.heap(i * 2 + 1) && (!heap.heap.isDefinedAt(i * 2 + 2) || heap.heap(i) <= heap.heap(i * 2 + 2)), i + " is not less than it's children in " + heap.heap)
    })

    (0 to vector.length - 1).foreach(i => assert(vector(i) === heap(i)))
  }

  test ("delete") {
    val vector = Vector(23,17,8, 5, 3, 2, 1)
    val (fullHeap, _) = vector.foldLeft((LookupHeap(), 0))((acc, weight) => acc match {
      case (h, count) => (h.insert(count, weight), count + 1)
    })

    val heap = fullHeap.delete(4)

    val firstRoot = (Math.floor(heap.heap.length / 2) - 1).toInt

    (firstRoot to 0 by -1).foreach(i => {
      assert(heap.heap(i) <= heap.heap(i * 2 + 1) && (!heap.heap.isDefinedAt(i * 2 + 2) || heap.heap(i) <= heap.heap(i * 2 + 2)), i + " is not less than it's children in " + heap.heap)
    })

    (0 to vector.length - 1).foreach(i =>
      if (i == 4) assert(heap.isDefinedAt(i) === false)
      else assert(vector(i) === heap(i)))
  }
}
