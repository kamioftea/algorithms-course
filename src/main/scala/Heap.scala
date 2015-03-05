import scala.annotation.tailrec

/**
 *
 * Created by Jeff on 04/03/2015.
 */
case class Heap[T](comp: (T,T) => Boolean)(heap: Vector[T] = Vector()) {
  lazy val size = heap.size
  lazy val top = heap(0)
  lazy val isEmpty = heap.isEmpty

  def swap(a: Int, b: Int): Heap[T] = {
    val heapSwap = heap(a)
    Heap(comp)(heap.updated(a, heap(b)).updated(b, heapSwap))
  }

  @tailrec
  final def heapifyDown(root_id: Int): Heap[T] = {
    val (left_id, right_id) = (2 * root_id + 1, 2 * root_id + 2)

    // is a leaf
    if (left_id >= heap.length)
      this
    // has only a left child
    else if (right_id == heap.length)
      if (comp(heap(root_id), heap(left_id)))
        swap(root_id, left_id)
      else
        this
    // is already minimum
    else if (comp(heap(left_id), heap(root_id)) && comp(heap(right_id),heap(root_id)))
      this
    // swap and bubble down with min child
    else if (comp(heap(left_id), heap(right_id)))
      swap(root_id, right_id).heapifyDown(right_id)
    else
      swap(root_id, left_id).heapifyDown(left_id)
  }

  def build: Heap[T] = {
    val firstRoot = (Math.floor(heap.length / 2) - 1).toInt

    (firstRoot to 0 by -1).foldLeft(Heap(comp)(heap))((h, root_id) => h.heapifyDown(root_id))
  }

  @tailrec
  final def heapifyUp(child: Int): Heap[T] = {
    if (child == 0) return this

    val parent = (Math.ceil(child.toFloat / 2) - 1).toInt

    if (comp(heap(parent), heap(child)))
      swap(parent, child).heapifyUp(parent)
    else
      this
  }

  def insert(min: T) = {
    val newIndex = heap.length
    Heap(comp)(heap :+ min).heapifyUp(newIndex)
  }

  private def removeLast() = Heap(comp)(heap.dropRight(1))

  def pop(): Option[(T, Heap[T])] =
    if (heap.isEmpty) None
    else Some(heap(0),swap(0, heap.length - 1).removeLast().heapifyDown(0))
}

object MinHeap {
  val empty = Heap[Int](_>_)()
}

object MaxHeap {
  val empty = Heap[Int](_<_)()
}
