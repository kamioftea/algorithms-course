/**
 *
 * Created by Jeff on 05/03/2015.
 */
case class State(sum: Int = 0, lowest: Heap[Int] = MaxHeap.empty, highest: Heap[Int] = MinHeap.empty)
{

  def apply(i: Int): State = {

    def balanceHeaps(l: Heap[Int], h: Heap[Int]): (Heap[Int], Heap[Int]) = {
      if (l.size < h.size) {
        h.pop() match {
          case Some((i, h1)) => (l.insert(i), h1)
        }
      }
      else if (l.size > h.size + 1)
      {
        l.pop() match {
          case Some((i, l1)) => (l1, h.insert(i))
        }
      }
      else (l,h)
    }

    val (l, h) = if (highest.isEmpty || highest.top > i)
       balanceHeaps(lowest.insert(i), highest)
    else
       balanceHeaps(lowest, highest.insert(i))

    State(sum + l.top, l, h)
  }
}

object MaintainMedian {
  def sumMedians(input: List[Int]): Int = {
    val finalState = input.foldLeft(State())((state, i) => state(i))
    finalState.sum
  }
}
