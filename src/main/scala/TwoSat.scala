import breeze.linalg.*

import scala.util.Random

/**
 *
 * Created by Jeff on 06/05/2015.
 */
case class TwoSat(buckets: Map[Int, Bucket]) {
  def withClause(clause: Clause): TwoSat =
    TwoSat(buckets
      .updated(clause.a.value, buckets(clause.a.value).addClause(clause))
      .updated(clause.b.value, buckets(clause.b.value).addClause(clause)))

  lazy val clauses = buckets.flatMap({ case (_, bucket) => bucket.hasClauses.toList ++ bucket.notClauses.toList }).toSet.toList

  def preFilter: TwoSat = {
    val redundant = buckets filter { case (_, bucket) => bucket.hasClauses.isEmpty || bucket.notClauses.isEmpty }
    if (redundant.isEmpty) this
    else TwoSat(redundant.foldLeft(buckets) {
      case (acc, (i, bucket)) => {
        val clauses = if (bucket.hasClauses.isEmpty) bucket.notClauses else bucket.hasClauses
        clauses.foldLeft(acc - i) {
          case (acc2, clause) => clause.getOther(i) match {
            case Some(v) if acc2.isDefinedAt(v.value) =>
              val updatedBucket = acc2(v.value).removeClause(clause)
              if (updatedBucket.isEmpty) acc2 - v.value
              else acc2.updated(v.value, updatedBucket)
            case _ => acc2
          }
        }
      }
    }).preFilter
  }

  def getUnsatisfiedClauses(assignment: Map[Int, Variable]) = clauses.filter(clause =>
    clause.a != assignment(clause.a.value) && clause.b != assignment(clause.b.value)
  )

  def randomAssignment: Map[Int, Variable] = {
    buckets.map({ case (i, _) => (i, if (Random.nextBoolean()) Has(i) else Not(i)) })
  }

  def improveAssignment(assignment: Map[Int, Variable], clauses: List[Clause]): Map[Int, Variable] = {
    val index = Random.nextInt(clauses.length)
    val clause = clauses(index)
    val variable =
      if (assignment(clause.a.value) == clause.a) clause.b
      else if (assignment(clause.b.value) == clause.b) clause.a
      else if (Random.nextBoolean()) clause.a
      else clause.b

    val (key: Int, value: Variable) =
      variable match {
        case Has(i) => (i, Has(i))
        case Not(i) => (i, Not(i))
      }

    assignment.updated(key, value)
  }

  def assignmentSatisfies(assignment: Map[Int, Variable]): Boolean = {
    def doAssignmentSatisfies(assignment: Map[Int, Variable], count: Int): Boolean = {
      if (count > clauses.size * 5) false
      else {
        val unsat = getUnsatisfiedClauses(assignment)
        if (unsat.isEmpty) true
        else doAssignmentSatisfies(improveAssignment(assignment, unsat), count + 1)
      }
    }

    doAssignmentSatisfies(assignment, 0)
  }

  def isSatisfiable: Boolean = {
    def doIsSatisfiable(count: Int): Boolean = {
      if (count > clauses.length * 5)
        false
      else if (assignmentSatisfies(randomAssignment))
        true
      else
        doIsSatisfiable(count + 1)
    }

    buckets.isEmpty || doIsSatisfiable(0)
  }

}

object TwoSat {
  def fromFile(headerFile: HeaderFile) = {
    val clause_count = headerFile.intHeaderAtOrElse("Clauses")
    val variable_count = headerFile.intHeaderAtOrElse("Variables", clause_count)

    val twoSat = new TwoSat((1 to variable_count + 1).map(i => (i, Bucket(i))).toMap)

    val linePattern = "(-?[\\d\\.]+)\\s+(-?[\\d\\.]+)".r

    headerFile.lines.foldLeft(twoSat)((twoSat, line) => line match {
      case linePattern(a, b) =>
        if (math.abs(a.toInt) < math.abs(b.toInt))
          twoSat.withClause(Clause(Variable(a.toInt), Variable(b.toInt)))
        else
          twoSat.withClause(Clause(Variable(b.toInt), Variable(a.toInt)))
    })
  }
}

object Variable {
  def apply(i: Int) = if (i > 0) Has(i) else Not(-i)
}

sealed abstract class Variable() {
  def value: Int
}

case class Has(value: Int) extends Variable {}

case class Not(value: Int) extends Variable {}

case class Clause(a: Variable, b: Variable) {
  assert(a.value < b.value, s"${a.value} is not less than ${b.value}")

  def get(i: Int): Option[Variable] =
    if (a.value == i) Some(a)
    else if (b.value == i) Some(b)
    else None

  def getOther(i: Int): Option[Variable] =
    if (a.value == i) Some(b)
    else if (b.value == i) Some(a)
    else None
}

case class Bucket(variable: Int, hasClauses: Set[Clause] = Set.empty[Clause], notClauses: Set[Clause] = Set.empty[Clause]) {
  def addClause(clause: Clause): Bucket = clause.get(variable) match {
    case Some(Has(_)) => Bucket(variable, hasClauses + clause, notClauses)
    case Some(Not(_)) => Bucket(variable, hasClauses, notClauses + clause)
    case None => throw new IndexOutOfBoundsException(s"Clause $clause must contain $variable")
  }

  def removeClause(clause: Clause) = clause.get(variable) match {
    case Some(Has(_)) => Bucket(variable, hasClauses - clause, notClauses)
    case Some(Not(_)) => Bucket(variable, hasClauses, notClauses - clause)
    case None => throw new IndexOutOfBoundsException(s"Clause $clause must contain $variable")
  }

  lazy val isEmpty = hasClauses.isEmpty && notClauses.isEmpty

}