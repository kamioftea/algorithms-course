import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by jeff on 04/03/2015.
 */
@RunWith(classOf[JUnitRunner])
class MyHashSetTest extends FunSuite {

  val firstSixPrimes = Vector(2,3,5,7,11,13)

  test("primes") {
    assert(MyHashSet.primes.take(6).toVector === firstSixPrimes)
  }
  test("next prime") {
    assert(MyHashSet.nextPrime(1000000) === 1000003)
  }
  test("contains true")
  {
    val hash = new MyHashSet(firstSixPrimes.map(BigInt(_)))
    assert(hash.contains(5) === true)
  }
  test("contains false")
  {
    val hash = new MyHashSet(firstSixPrimes.map(BigInt(_)))
    assert(hash.contains(5 + hash.size) === false)
  }
}
