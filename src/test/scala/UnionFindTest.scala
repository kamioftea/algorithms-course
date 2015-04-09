import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 *
 * Created by Jeff on 07/04/2015.
 */
@RunWith(classOf[JUnitRunner])
class UnionFindTest extends FunSuite {
  
  val initial = UnionFind.fromSeq(List(1,2,3,4))
  test("initally disjoint")
  {
    assert(initial.find(1) !== initial.find(2))
    assert(initial.find(2) !== initial.find(3))
    assert(initial.find(4) !== initial.find(3))
    assert(initial.find(4) !== initial.find(1))
  }
  
  val one_union = initial.union(2,3)
  
  test("after one union")
  {
    assert(one_union.find(1) !== one_union.find(2))
    assert(one_union.find(2) === one_union.find(3))
    assert(one_union.find(4) !== one_union.find(3))
    assert(one_union.find(4) !== one_union.find(1))
  }
  
  val two_union = one_union.union(1,4)

  test("after two union")
  {
    assert(two_union.find(1) !== two_union.find(2))
    assert(two_union.find(2) === two_union.find(3))
    assert(two_union.find(4) !== two_union.find(3))
    assert(two_union.find(4) === two_union.find(1))
  }

  val three_union = two_union.union(4,3)

  test("after three union")
  {
    assert(three_union.find(1) === three_union.find(2))
    assert(three_union.find(2) === three_union.find(3))
    assert(three_union.find(4) === three_union.find(3))
    assert(three_union.find(4) === three_union.find(1))
  }
  
  test("already unioned")
  {
    assert(three_union.union(1, 2) === three_union)
  }
  
  val alt_two_union = initial.union(1,2).union(3,2)
  test("with singleton")
  {
    assert(alt_two_union.find(1) === alt_two_union.find(2))
    assert(alt_two_union.find(2) === alt_two_union.find(3))
    assert(alt_two_union.find(4) !== alt_two_union.find(3))
    assert(alt_two_union.find(4) !== alt_two_union.find(1))
  }

}
