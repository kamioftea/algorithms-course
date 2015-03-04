import scala.annotation.tailrec
import scala.collection.immutable.HashSet

/**
 *
 *
 * Created by jeff on 04/03/2015.
 */
class TwoSum (items: Vector[BigInt]) {
  val hash = items.foldLeft(HashSet[BigInt]())(_ + _)
  val item_list = hash.toVector

  def hasSum(value: Int, possible: List[BigInt]):Boolean = {
    @tailrec
    def doHasSum(toCheck: List[BigInt]): Boolean = toCheck match {
      case Nil => false
      case x :: xs =>
        val inverse = value - x
        if (inverse != x && hash.contains(inverse)) true
        else doHasSum(xs)
    }

    doHasSum(possible)
  }

  def overRange(min: Int, max: Int) = {
    val factor = BigInt(MyHashSet.nextPrime(max - min + 1))
    val buckets = item_list.foldLeft(Map[Int, Set[BigInt]]())((map, i) => {
      val key = (i/factor).toInt
      if (map.isDefinedAt(key)) map.updated(key, map(key) + i)
      else map + (key -> Set(i))
    })
    val set = item_list.foldLeft(Set[Int]())((set, a) => {
      val key = ((min - a) / factor).toInt

      def mergeB(s: Set[Int], b: BigInt): Set[Int] =
        if(a!=b && min <= a+b && a+b <= max) s + (a + b).toInt else s

      def bucketOrNil(key: Int) = if (buckets.isDefinedAt(key)) buckets(key) else Nil

      bucketOrNil(key).foldLeft(bucketOrNil(key + 1).foldLeft(set)(mergeB))(mergeB)
    })

    set.size
  }
}

object TwoSum {
  def fromFile(file: HeaderFile) = new TwoSum(file.lines.toVector.map(x => BigInt(x)))
}
