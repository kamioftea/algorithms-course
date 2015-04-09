import scala.annotation.tailrec

/**
 *
 * Created by Jeff on 04/04/2015.
 */
case class LookupHeap(heap: Vector[Int] = Vector(), lookup: Map[Int, Int] = Map(), reverseLookup: Map[Int, Int] = Map()) {
    def isDefinedAt(i: Int): Boolean = lookup.isDefinedAt(i)

    def swap(a: Int, b: Int): LookupHeap = {
      LookupHeap(heap.updated(a, heap(b)).updated(b, heap(a)),
        lookup.updated(reverseLookup(a), b).updated(reverseLookup(b), a),
        reverseLookup.updated(a, reverseLookup(b)).updated(b, reverseLookup(a)))
    }

    def apply(i: Int) = heap(lookup(i))

    @tailrec
    final def heapifyDown(root_id: Int): LookupHeap = {
      val (left_id, right_id) = (2 * root_id + 1, 2 * root_id + 2)

      // is a leaf
      if (left_id >= heap.length)
        this
      // has only a left child
      else if (right_id == heap.length)
        if (heap(root_id) > heap(left_id))
          swap(root_id, left_id)
        else
          this
      // is already minimum
      else if (heap(root_id) <= heap(left_id) && heap(root_id) <= heap(right_id))
        this
      // swap and bubble down with min child
      else if (heap(left_id) > heap(right_id))
        swap(root_id, right_id).heapifyDown(right_id)
      else
        swap(root_id, left_id).heapifyDown(left_id)
    }

    def build: LookupHeap = {
      val firstRoot = Math.ceil(heap.length.toFloat / 2).toInt - 1

      (firstRoot to 0 by -1).foldLeft(this)((acc, root_id) => acc.heapifyDown(root_id))
    }

    @tailrec
    final def heapifyUp(child: Int): LookupHeap = {
      if (child == 0) return this

      val parent = Math.ceil(child.toFloat / 2).toInt - 1

      if (heap(parent) > heap(child))
        swap(parent, child).heapifyUp(parent)
      else
        this
    }

    def insert(ref: Int, value: Int) = {
      val newIndex = heap.length
      LookupHeap(heap :+ value, lookup + (ref -> newIndex), reverseLookup + (newIndex -> ref)).heapifyUp(newIndex)
    }

    private def removeLast() = {
      val index = heap.length - 1
      val ref = reverseLookup(index)
      LookupHeap(heap.dropRight(1), lookup - ref, reverseLookup - index)
    }

    def delete(ref: Int): LookupHeap = {
      val index = lookup(ref)

      if (index == heap.length - 1)
        removeLast()
      else
        swap(index, heap.length - 1).removeLast().heapifyDown(index)
    }

    def pop(): (Int, Int, LookupHeap) = {
      val (ref, weight) = (reverseLookup(0), heap(0))
      (ref, weight, delete(ref))
    }
  }

object LookupHeap {
  val empty = LookupHeap()
}
