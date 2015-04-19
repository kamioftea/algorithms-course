import scala.annotation.tailrec

/**
 *
 * Created by Jeff on 19/04/2015.
 */
object Knapsack {
  val linePattern = "(\\d+)\\s+(\\d+)".r

  def itemsFromFile(file: HeaderFile):List[Item] = file.lines map {
    case linePattern(v, w) => Item(v.toInt, w.toInt)
  }

  def maxValue(file: HeaderFile): BigInt = {
    val items = itemsFromFile(file).sortBy(-_.weight)
    val capacity = file.headerAt("capacity").map(_.toInt).getOrElse(0)
    items.foldLeft(Vector.fill(capacity)(BigInt(0)))((acc, item) => {
      @tailrec
      def doPopulateVector(vector: Vector[BigInt], position: Int): Vector[BigInt] = {
        if (position == vector.size) vector
        else {
          val previous = vector(position)
          val withThis = if(position - item.weight < 0) BigInt(0) else acc(position - item.weight) + item.value
          if (withThis > previous)
            doPopulateVector(vector.updated(position, withThis), position + 1)
          else
            doPopulateVector(vector, position + 1)
        }
      }

      doPopulateVector(acc, 0)
    })(capacity - 1)
  }
}

case class Item(value: Int, weight: Int);
