import scala.util.Random

/**
 *
 * Created by jeff on 04/03/2015.
 */
class MyHashSet(items: Vector[BigInt]) {

  val size = MyHashSet.nextPrime(items.size * 2)

  private val offset = BigInt(Random.nextInt(size))

  private def key(i: BigInt): Int = (i + offset).mod(BigInt(size)).toInt

  private val hash = items.foldLeft(Vector.fill[List[BigInt]](size)(Nil))((hash, i) => {
    val keyI = key(i)
    if(hash(keyI).contains(i)) hash
    else hash.updated(keyI, i :: hash(keyI))
  })

  def contains(i: BigInt) = hash(key(i)).contains(i)
}

object MyHashSet {
  lazy val primes: Stream[Int] = 2 #:: Stream.from(3).filter(isPrime)

  def isPrime(i: Int): Boolean = primes.takeWhile{j => j * j <= i}.forall{ k => i % k > 0}

  def nextPrime(n: Int) = Stream.from(n).filter(isPrime).head
}