/**
 *
 * Created by Jeff on 07/04/2015.
 */
class UnionFind[T](nodes: Map[T, Node[T]]){
  private def findNode(e: T): Node[T] = nodes.get(e) match {
    case Some(x) => x.leader match {
      case Some(l) => findNode(l)
      case None => x
    }
    case _ => throw new IndexOutOfBoundsException
  }
  
  def find(e: T) = findNode(e).elem
  
  def union(elem_a: T, elem_b: T): UnionFind[T] = {
    (nodes.get(elem_a), nodes.get(elem_b)) match {
      case (Some(a), Some(b)) =>
        val(l_a, l_b) = (findNode(a.elem), findNode(b.elem))
        if(l_a == l_b) this
        else {
          val (lg, sm) = if (l_a.children.size >= l_b.children.size) (l_a, l_b) else (l_b, l_a)
          new UnionFind[T]((sm.children + sm.elem).
            foldLeft(nodes.updated(lg.elem, Node[T](lg.elem, None, lg.children ++ sm.children + sm.elem)))
            ((acc, elem) => acc.updated(elem, Node[T](elem, Some(lg.elem))))
          )
        }
      case _ => this // Ignore missing values
    }
  }

  def leaders: Set[T] = nodes.map(x => find(x._1)).toSet

  override def toString = "UnionFind(" + nodes.toString + ")"
}

object UnionFind {
  def fromSeq[T](seq: Seq[T]): UnionFind[T] = new UnionFind[T](seq.map((e:T) => (e, Node[T](e))).toMap)
}

case class Node[T](elem: T, leader: Option[T] = None, children: Set[T] = Set[T]())
