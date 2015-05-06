import breeze.linalg.DenseMatrix
import breeze.util.ArrayUtil
import scala.collection.immutable.BitSet

class TSPGraph(val cities: Vector[City]) {

  import TSPGraph._

  val distances: Map[(Int, Int), Float] = (for {
    i <- 0 to cities.length - 1
    j <- i to cities.length - 1
  } yield (i, j) -> math.sqrt(
      math.pow(cities(i).x - cities(j).x, 2) +
        math.pow(cities(i).y - cities(j).y, 2)
    ).toFloat).toMap

  def distance(c1: Int, c2: Int): Float =
    if (c1 > c2) distance(c2, c1)
    else if (c2 >= cities.length) throw new IndexOutOfBoundsException(s"index $c2 must be less than cities.length ${cities.length}")
    else distances((c1, c2))

  def nextBitMask(x: Int): Int = {
    val y = x & -x
    val c = x + y
    (((x ^ c) >> 2) / y) | c
  }

  def sets(n: Int): Stream[Int] = {
    if (n >= math.pow(2, cities.length)) Stream.empty
    else Stream.cons(n, sets(nextBitMask(n)))
  }

  def getInitialMatrix = {
    val rows = math.pow(2, cities.length).toInt

    val cols = cities.length
    val data = new Array[Float](rows * cols)
    ArrayUtil.fill(data, 0, rows * cols, Float.PositiveInfinity)
    DenseMatrix.create[Float](math.pow(2, cities.length).toInt, cities.length, data)
  }

  def tsp: Int = {

    val matrix = getInitialMatrix

    matrix(1, 0) = 0

    (2 to cities.length).foreach(n => {
      println(s"Processing: $n")
      (for {
        set <- sets(math.pow(2, n).toInt - 1)
        j <- 1 to cities.length - 1
        if bitIntContains(set, 0)
        if bitIntContains(set, j)
      } yield (set, j)).foreach {
        case (set, j) => {
          val setWithoutJ = bitIntWithout(set, j)
          matrix(set, j) = (0 to cities.length - 1).filter(bitIntContains(set, _)).foldLeft(Float.PositiveInfinity) {
            case (prev, k) =>
              if (k == j) prev
              else {
                math.min(prev, matrix(setWithoutJ, k) + distance(k, j))
              }
          }
        }
      }
    })

    val res = (1 to cities.length - 1).foldLeft(Float.PositiveInfinity) {
      case (prev, k) =>
        math.min(prev, matrix(math.pow(2, cities.length).toInt - 1, k) + distance(0, k))

    }

    println(res)
    math.floor(res).toInt
  }
}

/**
 *
 * Created by Jeff on 03/05/2015.
 */
object TSPGraph {

  def fromFile(file: HeaderFile): TSPGraph = {
    val linePattern = "([\\d\\.]+)\\s+([\\d\\.]+)".r

    val cities = file.lines map {
      case linePattern(x, y) => City(x.toFloat, y.toFloat)
    }

    new TSPGraph(cities.toVector)
  }

  def toBitSet(n: Int): BitSet = BitSet.fromBitMask(Array(n))

  def toBitInt(s: BitSet): Int = s.toBitMask match {
    case Array(x) => x.toInt
  }

  def bitIntContains(s: Int, x: Int) = (s & (1 << x)) > 0

  def bitIntWithout(s: Int, x: Int) = s & ~(1 << x)

  def inverseBitInt(s: Int, n: Int): Int =
    ~s & (math.pow(2, n).toInt - 1)

  def inverseBitSet(s: BitSet, n: Int): BitSet = toBitSet(inverseBitInt(toBitInt(s), n))

}

case class City(x: Float, y: Float)
